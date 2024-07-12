use std::cell::Cell;
use std::rc::Rc;

use crate::wgpu::*;
use ::wgpu;
use uial::widget::{DynWidget, DynWidgetLayout};
use uial::drawer::*;
use uial::*;

/// Describes a self-contained application.
pub trait Application {
    /// Encapsulates the time-varying state of the application at any given moment.
    type State: HasReact + Track;

    /// Gets the title of the application.
    fn title(&self) -> &str;

    /// Gets the content of the application as a [`Widget`].
    fn body(&self, env: &RunEnv<Self::State>) -> Box<DynWidget<'_, RunEnv<Self::State>>>;

    /// Updates the state of the application in response to the passage of time.
    fn update(&self, env: &mut RunEnv<Self::State>, delta_time: Duration) {
        // Nothing done by default
        let _ = (env, delta_time);
    }
}

/// The type of environment provided by the runner while active.
#[repr(C)]
pub struct RunEnv<S: HasReact + Track + 'static> {
    // Here we lie about the lifetime of state reference. It is actually only valid for a little
    // longer than the enclosing `RunEnv`. Thus, the lifetime should be restricted appropriately
    // when accessed by an API.
    raw: RawRunEnv<'static, S>,

    // Make `RunEnv` unsized to prevent swapping between mutable references, which would make
    // the lifetime/mutability erasure above unsound.
    _unsized: [()],
}

impl<S: HasReact + Track> RunEnv<S> {
    /// Converts a [`RawRunEnv`] reference into a [`RunEnv`] reference.
    fn from_raw<'a>(raw: &'a RawRunEnv<S>) -> &'a RunEnv<S> {
        unsafe { &*(core::ptr::slice_from_raw_parts(raw, 0) as *const RunEnv<S>) }
    }

    /// Converts a mutable [`RawRunEnv`] reference into a [`RunEnv`] reference.
    fn from_raw_mut<'a>(raw: &'a mut RawRunEnv<S>) -> &'a mut RunEnv<S> {
        unsafe { &mut *(core::ptr::slice_from_raw_parts_mut(raw, 0) as *mut RunEnv<S>) }
    }

    /// Gets a reference to the user-defined application state.
    pub fn state(&self) -> &S {
        self.raw.state
    }

    /// Gets a mutable reference to the user-defined application state.
    pub fn state_mut(&mut self) -> &mut S {
        self.raw.state
    }
}

/// The internal data for [`RunEnv`].
struct RawRunEnv<'state, S: HasReact + Track + 'static> {
    image_atlas: &'static WgpuImageAtlas<'static>,
    drawer_context: &'static WgpuDrawerContext<'static>,
    state: &'state mut S,
    cursor: &'static ReactCell<S::React, Option<Rc<RunCursor<S>>>>,
    root: Option<&'static RunWidgetRoot<S>>,
}

impl<S: HasReact + Track> HasWgpuContext<'static> for RunEnv<S> {
    fn wgpu_context(&self) -> &'static WgpuContext {
        self.raw.image_atlas.context()
    }
}

impl<S: HasReact + Track> HasWgpuDrawerContext<'static> for RunEnv<S> {
    fn wgpu_drawer_context(&self) -> &'static WgpuDrawerContext<'static> {
        self.raw.drawer_context
    }
}

impl<S: HasReact + Track> HasImageManager for RunEnv<S> {
    type ImageManager = &'static WgpuImageAtlas<'static>;
    fn image_manager(&self) -> Self::ImageManager {
        self.raw.image_atlas
    }
}

impl<S: HasReact + Track> HasReact for RunEnv<S> {
    type React = S::React;

    fn react(&self) -> &S::React {
        self.raw.state.react()
    }

    fn react_mut(&mut self) -> &mut S::React {
        self.raw.state.react_mut()
    }
}

impl<S: HasReact + HasClock + Track> HasClock for RunEnv<S> {
    fn clock(&self) -> Instant {
        self.raw.state.clock()
    }
}

impl<S: HasReact + Track> Track for RunEnv<S> {
    type ValidityToken = S::ValidityToken;

    fn track<R>(&self, inner: impl FnOnce() -> R) -> (R, Self::ValidityToken) {
        self.raw.state.track(inner)
    }

    fn is_valid(&self, token: &Self::ValidityToken) -> bool {
        self.raw.state.is_valid(token)
    }
}

impl<S: HasReact + Track> WidgetEnvironment for RunEnv<S> {
    type Drawer = WgpuErasedDrawer;

    fn interactions(&self, mut f: impl FnMut(&dyn Interaction)) {
        if let Some(cursor) = self.raw.cursor.get(self) {
            cursor.handler.get(self).interactions(self, &mut f);
        }
    }

    fn with_locate<'a, W: Widget<Self>, R>(
        &self,
        widget: &'a W,
        f: impl FnOnce(WidgetInst<'a, '_, W>) -> R,
    ) -> Option<R> {
        let mut outline = WidgetOutline::new();
        let inst = self.raw.root?.inst(self);
        inst.inner().outline(&mut outline);
        outline.locate(widget).map(f)
    }
}

/// Encapsulates the information needed to obtain a [`WidgetInst`] for the root widget.
struct RunWidgetRoot<S: HasReact + Track + 'static> {
    widget: Box<DynWidget<'static, RunEnv<S>>>,
    size: ReactCell<S::React, Size2i>,
    layout_cache: Cache<RunEnv<S>, Rc<Box<DynWidgetLayout<'static, RunEnv<S>>>>>,
}

impl<S: HasReact + Track + 'static> RunWidgetRoot<S> {
    /// Gets the current layout of the widget.
    pub fn layout(&self, env: &RunEnv<S>) -> Rc<Box<DynWidgetLayout<'static, RunEnv<S>>>> {
        self.layout_cache.get(env, |env, layout| {
            let size = self.size.get(env);
            if let Some(layout) = layout {
                if let Ok(mut layout) = Rc::try_unwrap(layout) {
                    self.widget.relayout(&mut layout, env, size);
                    Rc::new(layout)
                } else {
                    Rc::new(self.widget.layout(env, size))
                }
            } else {
                Rc::new(self.widget.layout(env, size))
            }
        })
    }

    /// Gets the current [`RunWidgetRootInst`] for this widget.
    pub fn inst(&self, env: &RunEnv<S>) -> RunWidgetRootInst<S> {
        RunWidgetRootInst {
            widget: &self.widget,
            layout: self.layout(env),
        }
    }
}

/// A wrapper over a [`WidgetInst`] for a [`RunWidgetRoot`].
struct RunWidgetRootInst<'a, S: HasReact + Track + 'static> {
    widget: &'a DynWidget<'static, RunEnv<S>>,
    layout: Rc<Box<DynWidgetLayout<'static, RunEnv<S>>>>,
}

impl<'a, S: HasReact + Track> RunWidgetRootInst<'a, S> {
    /// Gets the [`WidgetInst`] for this [`RunWidgetRootInst`].
    pub fn inner(&self) -> WidgetInst<'a, '_, DynWidget<'static, RunEnv<S>>> {
        WidgetInst {
            widget: self.widget,
            min: vec2i(0, 0),
            layout: &self.layout,
        }
    }
}

/// A cursor for a [`RunEnv`].
pub struct RunCursor<S: HasReact + Track + 'static> {
    pos: ReactCell<S::React, Point2i>,
    handler: ReactCell<S::React, Rc<DynCursorHandler<'static, RunEnv<S>>>>,
    keyboard: &'static RunKeyboard<S>,
}

impl<S: HasReact + Track> Cursor<'static, RunEnv<S>> for RunCursor<S> {
    fn pos(&self, env: &RunEnv<S>) -> Point2i {
        self.pos.get(env)
    }

    type Keyboard = &'static RunKeyboard<S>;
    fn keyboard(&self, _: &RunEnv<S>) -> Option<Self::Keyboard> {
        Some(self.keyboard)
    }

    fn set_handler(&self, env: &mut RunEnv<S>, handler: impl CursorHandler<RunEnv<S>> + 'static) {
        self.handler
            .set(env, DynCursorHandler::from_rc(Rc::new(handler)))
    }

    fn clear_handler(&self, env: &mut RunEnv<S>) {
        // SAFETY: `cursor` doesn't actually live for `'static`, but since this reference can't
        // escape outside the cursor itself, it won't be dereferenced after the cursor is dropped.
        let cursor: &'static Self = unsafe { std::mem::transmute(self) };
        self.handler.set(
            env,
            DynCursorHandler::from_rc(Rc::new(DefaultHandler(cursor))),
        );
    }

    fn default_interactions(
        &self,
        env: &RunEnv<S>,
        mut f: impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        // SAFETY: `cursor` doesn't actually live for `'static`, but since this reference can't
        // escape outside the cursor itself, it won't be dereferenced after the cursor is dropped.
        let cursor: &'static Self = unsafe { std::mem::transmute(self) };
        if let Some(root) = env.raw.root {
            root.inst(env)
                .inner()
                .hover_interactions(env, cursor, &mut f)
        } else {
            EventStatus::Bubble
        }
    }

    fn default_mouse_scroll(&self, env: &mut RunEnv<S>, amount: ScrollAmount) -> EventStatus {
        // SAFETY: `cursor` doesn't actually live for `'static`, but since this reference can't
        // escape outside the cursor itself, it won't be dereferenced after the cursor is dropped.
        let cursor: &'static Self = unsafe { std::mem::transmute(self) };
        if let Some(root) = env.raw.root {
            root.inst(env).inner().mouse_scroll(env, cursor, amount)
        } else {
            EventStatus::Bubble
        }
    }

    fn default_mouse_down(&self, env: &mut RunEnv<S>, button: MouseButton) -> EventStatus {
        // SAFETY: `cursor` doesn't actually live for `'static`, but since this reference can't
        // escape outside the cursor itself, it won't be dereferenced after the cursor is dropped.
        let cursor: &'static Self = unsafe { std::mem::transmute(self) };
        if let Some(root) = env.raw.root {
            root.inst(env).inner().mouse_down(env, cursor, button)
        } else {
            EventStatus::Bubble
        }
    }
}

/// Represents the keyboard in a [`RunEnv`].
pub struct RunKeyboard<S: HasReact + Track + 'static> {
    keys_held: ReactCell<S::React, Vec<Key>>,
    handler: ReactCell<S::React, Rc<DynKeyboardHandler<'static, RunEnv<S>>>>,
}

impl<S: HasReact + Track> Keyboard<'static, RunEnv<S>> for RunKeyboard<S> {
    fn keys_held(&self, env: &RunEnv<S>, held: impl FnMut(Key)) {
        self.keys_held
            .with_ref(env, |keys_held| keys_held.iter().copied().for_each(held))
    }

    fn set_handler(&self, env: &mut RunEnv<S>, handler: impl KeyboardHandler<RunEnv<S>> + 'static) {
        // TODO: Avoid double-wrapping
        self.handler.set(env, Rc::new(handler))
    }

    fn default_key_down(&self, _: &mut RunEnv<S>, _: Key) -> EventStatus {
        // Nothing done for default handler
        EventStatus::Bubble
    }
}

/// Initializes and runs an [`Application`].
pub fn run<App: Application + 'static>(app: App, mut state: App::State) -> ! {
    let event_loop = winit::event_loop::EventLoop::new();
    let window = winit::window::WindowBuilder::new()
        .with_title(app.title())
        .build(&event_loop)
        .unwrap();
    let inner = async {
        let size = window.inner_size();
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });
        let surface = unsafe { instance.create_surface(&window).unwrap() };
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                force_fallback_adapter: false,
                compatible_surface: Some(&surface),
            })
            .await
            .expect("Failed to find an appropriate adapter");
        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features: wgpu::Features::empty(),
                    limits: wgpu::Limits::default().using_resolution(adapter.limits()),
                },
                None,
            )
            .await
            .expect("Failed to create device");
        let surface_capabilities = surface.get_capabilities(&adapter);
        let draw_format = surface_capabilities.formats[0];
        let mut config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: draw_format,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: surface_capabilities.alpha_modes[0],
            view_formats: vec![],
        };
        surface.configure(&device, &config);
        let drawer_resources = Cell::<Option<WgpuDrawerResources>>::new(None);
        let mut context = Extender::new(WgpuContext { device, queue });
        let mut image_atlas = Extender::new(WgpuImageAtlas::new(Extender::as_ref(&context)));
        let mut drawer_context = Extender::new(WgpuDrawerContext::new(
            Extender::as_ref(&context),
            Extender::as_ref(&image_atlas),
            draw_format,
        ));
        let mut cursor = Extender::new(
            state
                .react()
                .new_cell::<Option<Rc<RunCursor<App::State>>>>(None),
        );
        let mut keyboard = Extender::new(RunKeyboard {
            keys_held: state.react().new_cell(Vec::new()),
            handler: state.react().new_cell(Rc::new(())),
        });

        // Initialize application
        let mut app = Extender::new(app);
        let mut root = Extender::new(RunWidgetRoot {
            widget: Extender::as_ref(&app).body(RunEnv::from_raw(&RawRunEnv {
                image_atlas: Extender::as_ref(&image_atlas),
                drawer_context: Extender::as_ref(&drawer_context),
                state: &mut state,
                cursor: Extender::as_ref(&cursor),
                root: None,
            })),
            size: state.react().new_cell(size2i(size.width, size.height)),
            layout_cache: Cache::new(),
        });

        // Apply min/max window size
        // TODO: Update when changed
        let sizing = root.widget.sizing(RunEnv::from_raw(&RawRunEnv {
            image_atlas: Extender::as_ref(&image_atlas),
            drawer_context: Extender::as_ref(&drawer_context),
            state: &mut state,
            cursor: Extender::as_ref(&cursor),
            root: None,
        }));
        if let Some((min, max)) = sizing.as_range() {
            window.set_min_inner_size(Some(winit::dpi::PhysicalSize::new(min.x, min.y)));
            window.set_max_inner_size(if max.x < u32::MAX && max.y < u32::MAX {
                Some(winit::dpi::PhysicalSize::new(max.x, max.y))
            } else {
                None
            });
        }

        // Begin main loop
        let mut prev_time = std::time::Instant::now();
        event_loop.run(move |event, _, control_flow| {
            use winit::event::*;
            use winit::event_loop::*;
            let _ = (&instance, &adapter, &window);
            let mut raw_env = RawRunEnv {
                image_atlas: Extender::as_ref(&image_atlas),
                drawer_context: Extender::as_ref(&drawer_context),
                state: &mut state,
                cursor: Extender::as_ref(&cursor),
                root: Some(Extender::as_ref(&root)),
            };
            let env = RunEnv::from_raw_mut(&mut raw_env);
            let cur_time = std::time::Instant::now();
            app.update(env, (cur_time - prev_time).try_into().unwrap());
            prev_time = cur_time;
            *control_flow = ControlFlow::Poll;
            match event {
                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::Resized(n_size) => {
                        config.width = n_size.width;
                        config.height = n_size.height;
                        root.size.set(env, size2i(n_size.width, n_size.height));
                        surface.configure(&context.device, &config);
                        window.request_redraw();
                    }
                    WindowEvent::Focused(focused) => {
                        if focused {
                            Extender::as_ref(&root).inst(env).inner().focus(
                                env,
                                Extender::as_ref(&keyboard),
                                false,
                            );
                        } else {
                            keyboard.handler.set(env, Rc::new(()));
                        }
                    }
                    WindowEvent::KeyboardInput { input, .. } => {
                        let key = Key {
                            scan_code: input.scancode,
                            virtual_key_code: input.virtual_keycode,
                        };
                        let handler = keyboard.handler.get(env);
                        match input.state {
                            ElementState::Pressed => {
                                keyboard.keys_held.with_mut(env, |keys_held| {
                                    keys_held.retain(|k| *k != key);
                                    keys_held.push(key);
                                });
                                handler.key_down(env, key)
                            }
                            ElementState::Released => {
                                keyboard.keys_held.with_mut(env, |keys_held| {
                                    keys_held.retain(|k| *k != key);
                                });
                                handler.key_up(env, key)
                            }
                        }
                    }
                    WindowEvent::CursorMoved { position, .. } => {
                        let pos =
                            vec2i(position.x as i32, config.height as i32 - position.y as i32);
                        if let Some(cursor) = cursor.get(env) {
                            cursor.pos.set(env, pos);
                        } else {
                            let n_cursor = Rc::new(RunCursor {
                                pos: env.react().new_cell(pos),
                                handler: env
                                    .react()
                                    .new_cell(DynCursorHandler::from_rc(Rc::new(()))),
                                keyboard: Extender::as_ref(&keyboard),
                            });
                            n_cursor.clear_handler(env);
                            cursor.set(env, Some(n_cursor));
                        }
                    }
                    WindowEvent::CursorLeft { .. } => {
                        cursor.set(env, None);
                    }
                    WindowEvent::MouseWheel { delta, .. } => {
                        let amount = match delta {
                            MouseScrollDelta::LineDelta(x, y) => ScrollAmount::Ticks([x, y]),
                            MouseScrollDelta::PixelDelta(delta) => {
                                ScrollAmount::Pixels([delta.x as f32, delta.y as f32])
                            }
                        };
                        if let Some(cursor) = cursor.get(env) {
                            cursor.handler.get(env).mouse_scroll(env, amount);
                        }
                    }
                    WindowEvent::MouseInput { state, button, .. } => {
                        if let Some(cursor) = cursor.get(env) {
                            let handler = cursor.handler.get(env);
                            match state {
                                ElementState::Pressed => handler.mouse_down(env, button),
                                ElementState::Released => handler.mouse_up(env, button),
                            }
                        }
                    }
                    WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                    _ => {}
                },
                Event::MainEventsCleared => {
                    window.request_redraw();
                }
                Event::RedrawRequested(_) => {
                    let frame = surface
                        .get_current_texture()
                        .expect("Failed to acquire next swap chain texture");
                    let view = frame
                        .texture
                        .create_view(&wgpu::TextureViewDescriptor::default());
                    let size = size2i(config.width, config.height);
                    let mut resources = match drawer_resources.take() {
                        Some(resources) if resources.size() == size => resources,
                        _ => WgpuDrawerResources::new(Extender::as_ref(&drawer_context), size),
                    };
                    drawer_context.draw_to(&mut resources, &view, |drawer| {
                        root.inst(env)
                            .inner()
                            .draw(env, WgpuErasedDrawer::from_mut(drawer))
                    });
                    drawer_resources.set(Some(resources));
                    frame.present();
                }
                Event::LoopDestroyed => {
                    drop(drawer_resources.take());
                    unsafe {
                        Extender::drop(&mut keyboard);
                        Extender::drop(&mut cursor);
                        Extender::drop(&mut root);
                        Extender::drop(&mut app);
                        Extender::drop(&mut drawer_context);
                        Extender::drop(&mut image_atlas);
                        Extender::drop(&mut context);
                    }
                }
                _ => {}
            }
        });
    };
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = env_logger::try_init();
        pollster::block_on(inner)
    }
    #[cfg(target_arch = "wasm32")]
    {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_log::init().expect("could not initialize logger");
        use winit::platform::web::WindowExtWebSys;
        // On wasm, append the canvas to the document body
        web_sys::window()
            .and_then(|win| win.document())
            .and_then(|doc| doc.body())
            .and_then(|body| {
                body.append_child(&web_sys::Element::from(window.canvas()))
                    .ok()
            })
            .expect("couldn't append canvas to document body");
        wasm_bindgen_futures::spawn_local(inner);
    }
}

/// Wraps an enclosed value of type `T` and provides a `'static` reference to it.
///
/// The enclosed value may be explicitly dropped by calling [`Extender::drop`], but it is up
/// to the caller to ensure that the value is not used after it is dropped.
struct Extender<T>(*mut T);

impl<T> Extender<T> {
    /// Constructs a new [`Extender`] wrapper over the given value.
    pub fn new(value: T) -> Self {
        Self(Box::into_raw(Box::new(value)))
    }

    /// Gets a `'static` reference to the enclosed value of an [`Extender`].
    pub fn as_ref(ext: &Self) -> &'static T {
        unsafe { &*ext.0 }
    }

    /// Drops the enclosed value of an [`Extender`].
    /// 
    /// ## Safety
    /// The caller must ensure that the enclosed value is not accessed after this call, and that
    /// this is called at most once.
    pub unsafe fn drop(ext: &mut Self) {
        drop(Box::from_raw(ext.0))
    }
}

impl<T: 'static> std::ops::Deref for Extender<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        Self::as_ref(self)
    }
}
