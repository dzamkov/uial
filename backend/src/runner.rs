use crate::wgpu::*;
use ::wgpu;
use std::cell::Cell;
use std::rc::Rc;
use uial::drawer::*;
use uial::*;

/// Describes a self-contained application.
pub trait Application {
    /// Encapsulates the time-varying state of the application at any given moment.
    type State: HasReact + Track;

    /// Gets the title of the application.
    fn title(&self) -> &str;

    /// Gets the content of the application as a [`Widget`].
    fn body(&self, env: &RunEnv<Self::State>) -> impl Widget<RunEnv<Self::State>> + '_;

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
}

struct RunWidgetSlot<S: HasReact + Track + 'static> {
    size: &'static ReactCell<S::React, Size2i>,
}

impl<S: HasReact + Track + 'static> Clone for RunWidgetSlot<S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S: HasReact + Track + 'static> Copy for RunWidgetSlot<S> {}

/// The type of [`WidgetSlot`] provided to the top-level [`WidgetInst`].
impl<S: HasReact + Track + 'static> WidgetSlot<RunEnv<S>> for RunWidgetSlot<S> {
    fn is_visible(&self, env: &RunEnv<S>) -> bool {
        // TODO: Should be false while the window is minimized
        true
    }

    fn size(&self, env: &RunEnv<S>) -> Size2i {
        self.size.get(env)
    }

    fn min(&self, _: &RunEnv<S>) -> Point2i {
        vec2i(0, 0)
    }

    fn bubble_general_event(&self, _: &mut RunEnv<S>, _: GeneralEvent) {
        // Nothing to do here
    }
}

/// A set of interaction handlers with disjoint [`FocusScope`].
struct InteractionHandlerSet<'ui, Env: WidgetEnvironment + ?Sized> {
    cursor: Option<CursorInteractionRequest<'ui, Env>>,
    focus: Vec<FocusInteractionRequest<'ui, Env>>,
}

impl<'ui, Env: WidgetEnvironment + ?Sized> InteractionHandlerSet<'ui, Env> {
    /// Constructs a new [`InteractionHandlerSet`].
    pub fn new() -> Self {
        Self {
            cursor: None,
            focus: Vec::new(),
        }
    }

    /// Installs a cursor interaction handler, evicting any existing handler with overlapping
    /// scope.
    pub fn install_cursor(&mut self, req: CursorInteractionRequest<'ui, Env>) {
        self.remove(req.scope);
        self.cursor = Some(req);
    }

    /// Installs a focus interaction handler, evicting any existing handler with overlapping scope.
    pub fn install_focus(&mut self, req: FocusInteractionRequest<'ui, Env>) {
        self.remove(req.scope);
        self.focus.push(req);
    }

    /// Gets the cursor interaction handler, if one is installed.
    pub fn cursor(&self) -> Option<Rc<dyn CursorInteractionHandler<'ui, Env> + 'ui>> {
        self.cursor.as_ref().map(|req| req.handler.clone())
    }

    /// Gets the interaction handler, if any, that overlaps the given [`FocusScope`].
    pub fn get(&self, aspect: FocusScope) -> Option<DynInteractionHandler<'ui, Env>> {
        if let Some(cursor) = &self.cursor {
            if cursor.scope.overlaps(aspect) {
                return Some(DynInteractionHandler::Cursor(cursor.handler.clone()));
            }
        }
        for focus in &self.focus {
            if focus.scope.overlaps(aspect) {
                return Some(DynInteractionHandler::Focus(focus.handler.clone()));
            }
        }
        None
    }

    /// Removes the cursor interaction handler, if one is installed.
    pub fn remove_cursor(&mut self) {
        self.cursor = None;
    }

    /// Removes all interaction handlers whose [`FocusScope`] overlaps the given [`FocusScope`]
    /// from this set.
    pub fn remove(&mut self, aspect: FocusScope) {
        if let Some(cursor) = &self.cursor {
            if cursor.scope.overlaps(aspect) {
                self.cursor = None;
            }
        }
        self.focus.retain(|f| !f.scope.overlaps(aspect));
    }

    /// Removes all interaction handlers from this set.
    pub fn clear(&mut self) {
        self.cursor = None;
        self.focus.clear();
    }
}

/// Identifies some kind of interaction handler.
enum DynInteractionHandler<'ui, Env: WidgetEnvironment + ?Sized> {
    Cursor(Rc<dyn CursorInteractionHandler<'ui, Env> + 'ui>),
    Focus(Rc<dyn FocusInteractionHandler<'ui, Env> + 'ui>),
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
        let drawer_resources = Cell::<Option<WgpuDrawerResources>>::new(None);
        let mut context = Extender::new(WgpuContext { device, queue });
        let mut image_atlas = Extender::new(WgpuImageAtlas::new(Extender::as_ref(&context)));
        let mut drawer_context = Extender::new(WgpuDrawerContext::new(
            Extender::as_ref(&context),
            Extender::as_ref(&image_atlas),
            draw_format,
        ));

        // Initialize application
        let mut app = Extender::new(app);
        let widget = Extender::new(Extender::as_ref(&app).body(RunEnv::from_raw(&RawRunEnv {
            image_atlas: Extender::as_ref(&image_atlas),
            drawer_context: Extender::as_ref(&drawer_context),
            state: &mut state,
        })));
        let sizing = widget.sizing(RunEnv::from_raw(&RawRunEnv {
            image_atlas: Extender::as_ref(&image_atlas),
            drawer_context: Extender::as_ref(&drawer_context),
            state: &mut state,
        }));

        // Apply min/max window size
        // TODO: Update when changed
        let mut size = size2i(size.width, size.height);
        if let Some((min, max)) = sizing.as_range() {
            size.x = u32::clamp(size.x, min.x, max.x);
            size.y = u32::clamp(size.y, min.y, max.y);
            window.set_inner_size(winit::dpi::PhysicalSize::new(size.x, size.y));
            window.set_min_inner_size(Some(winit::dpi::PhysicalSize::new(min.x, min.y)));
            window.set_max_inner_size(if max.x < u32::MAX && max.y < u32::MAX {
                Some(winit::dpi::PhysicalSize::new(max.x, max.y))
            } else {
                None
            });
        }

        // Configure surface
        let mut config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: draw_format,
            width: size.x,
            height: size.y,
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: surface_capabilities.alpha_modes[0],
            view_formats: vec![],
        };
        surface.configure(&context.device, &config);

        // Initialize widget
        let mut size = Extender::new(state.react().new_cell(size));
        let mut inst = Extender::new(Extender::as_ref(&widget).inst(
            RunEnv::from_raw(&RawRunEnv {
                image_atlas: Extender::as_ref(&image_atlas),
                drawer_context: Extender::as_ref(&drawer_context),
                state: &mut state,
            }),
            RunWidgetSlot {
                size: Extender::as_ref(&size),
            },
        ));

        // Initialize interaction handlers
        let mut handlers = Extender::new(state.react().new_cell(InteractionHandlerSet::new()));

        // Begin main loop
        let mut cursor_pos = None;
        let mut prev_time = std::time::Instant::now();
        let mut is_cursor_locked = false;
        event_loop.run(move |event, _, control_flow| {
            use winit::event::*;
            use winit::event_loop::*;
            let _ = (&instance, &adapter, &window);
            let mut raw_env = RawRunEnv {
                image_atlas: Extender::as_ref(&image_atlas),
                drawer_context: Extender::as_ref(&drawer_context),
                state: &mut state,
            };
            let env = RunEnv::from_raw_mut(&mut raw_env);
            let cur_time = std::time::Instant::now();
            app.update(env, (cur_time - prev_time).try_into().unwrap());
            prev_time = cur_time;
            *control_flow = ControlFlow::Poll;

            // Respond to event
            #[allow(clippy::collapsible_match)]
            match event {
                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::Resized(n_size) => {
                        if n_size.width > 0 && n_size.height > 0 {
                            config.width = n_size.width;
                            config.height = n_size.height;
                            size.set(env, size2i(n_size.width, n_size.height));
                            surface.configure(&context.device, &config);
                            window.request_redraw();
                        }
                    }
                    WindowEvent::Focused(focused) => {
                        if focused {
                            if let Some(req) = Extender::as_ref(&inst).focus(env, false) {
                                handlers.with_mut(env, |handlers| {
                                    handlers.install_focus(req);
                                });
                            }
                        } else {
                            handlers.with_mut(env, |handlers| {
                                handlers.clear();
                            });
                        }
                    }
                    WindowEvent::KeyboardInput { input, .. } => {
                        process_general_event(
                            env,
                            &handlers,
                            cursor_pos,
                            GeneralEvent::Key {
                                key: Key {
                                    scan_code: input.scancode,
                                    virtual_key_code: input.virtual_keycode,
                                },
                                is_down: input.state == ElementState::Pressed,
                            },
                        );
                    }
                    WindowEvent::CursorMoved { position, .. } => {
                        cursor_pos = Some(vec2i(
                            position.x as i32,
                            config.height as i32 - position.y as i32,
                        ));
                    }
                    WindowEvent::CursorLeft { .. } => {
                        cursor_pos = None;
                    }
                    WindowEvent::MouseWheel { delta, .. } => {
                        process_cursor_event(
                            env,
                            Extender::as_ref(&inst),
                            &handlers,
                            cursor_pos,
                            CursorEvent::MouseScroll(delta),
                        );
                    }
                    WindowEvent::MouseInput { state, button, .. } => {
                        process_cursor_event(
                            env,
                            Extender::as_ref(&inst),
                            &handlers,
                            cursor_pos,
                            CursorEvent::MouseButton {
                                button,
                                is_down: state == ElementState::Pressed,
                            },
                        );
                    }
                    WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                    _ => {}
                },
                Event::DeviceEvent { event, .. } =>
                {
                    #[allow(clippy::single_match)]
                    match event {
                        DeviceEvent::MouseMotion { delta } => {
                            process_cursor_event(
                                env,
                                Extender::as_ref(&inst),
                                &handlers,
                                cursor_pos,
                                CursorEvent::Motion(vec2(delta.0 as Scalar, -delta.1 as Scalar)),
                            );
                        }
                        _ => {}
                    }
                }
                Event::MainEventsCleared => {
                    window.request_redraw();
                }
                Event::RedrawRequested(_) => {
                    let frame = match surface.get_current_texture() {
                        Ok(frame) => frame,
                        Err(wgpu::SurfaceError::Timeout) => surface
                            .get_current_texture()
                            .expect("Failed to acquire next swap chain texture"),
                        Err(_) => {
                            surface.configure(&context.device, &config);
                            surface
                                .get_current_texture()
                                .expect("Failed to acquire next swap chain texture")
                        }
                    };
                    let view = frame
                        .texture
                        .create_view(&wgpu::TextureViewDescriptor::default());
                    let size = size2i(config.width, config.height);
                    let mut resources = match drawer_resources.take() {
                        Some(resources) if resources.size() == size => resources,
                        _ => WgpuDrawerResources::new(Extender::as_ref(&drawer_context), size),
                    };
                    drawer_context.draw_to(&mut resources, &view, |drawer| {
                        inst.draw(env, WgpuErasedDrawer::from_mut(drawer))
                    });
                    drawer_resources.set(Some(resources));
                    frame.present();
                }
                Event::LoopDestroyed => {
                    drop(drawer_resources.take());
                    unsafe {
                        Extender::drop(&mut handlers);
                        Extender::drop(&mut inst);
                        Extender::drop(&mut size);
                        Extender::drop(&mut app);
                        Extender::drop(&mut drawer_context);
                        Extender::drop(&mut image_atlas);
                        Extender::drop(&mut context);
                    }
                }
                _ => {}
            }

            // Update UI state
            let n_is_cursor_locked = handlers.with_ref(env, |handlers| {
                handlers
                    .cursor()
                    .map_or(false, |handler| handler.is_locked(env))
            });
            if n_is_cursor_locked != is_cursor_locked {
                is_cursor_locked = n_is_cursor_locked;
                if is_cursor_locked {
                    let _ = window
                        .set_cursor_grab(winit::window::CursorGrabMode::Confined)
                        .or_else(|_e| {
                            window.set_cursor_grab(winit::window::CursorGrabMode::Locked)
                        });
                    window.set_cursor_visible(false);
                } else {
                    let _ = window.set_cursor_grab(winit::window::CursorGrabMode::None);
                    window.set_cursor_visible(true);
                }
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

/// Processes a cursor event
fn process_cursor_event<'ui, S: HasReact + Track>(
    env: &mut RunEnv<S>,
    inst: &'ui impl WidgetInst<RunEnv<S>>,
    handlers: &ReactCell<S::React, InteractionHandlerSet<'ui, RunEnv<S>>>,
    pos: Option<Point2i>,
    event: CursorEvent,
) {
    let handler = handlers.with_ref(env, |handlers| handlers.cursor());
    match handler {
        Some(handler) => match handler.cursor_event(env, pos.unwrap(), event) {
            CursorInteractionEventResponse::Keep => {}
            CursorInteractionEventResponse::Replace(req) => {
                handlers.with_mut(env, |handlers| {
                    handlers.install_cursor(req);
                });
            }
            CursorInteractionEventResponse::Downgrade(req) => {
                handlers.with_mut(env, |handlers| {
                    handlers.install_focus(req);
                });
            }
            CursorInteractionEventResponse::End => {
                handlers.with_mut(env, |handlers| {
                    handlers.remove_cursor();
                });
            }
        },
        None => {
            if let Some(pos) = pos {
                let res = inst.cursor_event(env, pos, event);
                if let widget::CursorEventResponse::Start(req) = res {
                    handlers.with_mut(env, |handlers| {
                        handlers.install_cursor(req);
                    });
                }
            }
        }
    }
}

/// Processes a general event
fn process_general_event<S: HasReact + Track>(
    env: &mut RunEnv<S>,
    handlers: &ReactCell<S::React, InteractionHandlerSet<RunEnv<S>>>,
    cursor_pos: Option<Point2i>,
    event: GeneralEvent,
) {
    let handler = handlers.with_ref(env, |handlers| handlers.get(FocusScope::KEYBOARD));
    match handler {
        Some(DynInteractionHandler::Cursor(handler)) => {
            match handler.general_event(env, cursor_pos.unwrap(), event) {
                CursorInteractionEventResponse::Keep => {}
                CursorInteractionEventResponse::Replace(req) => {
                    handlers.with_mut(env, |handlers| {
                        handlers.install_cursor(req);
                    });
                }
                CursorInteractionEventResponse::Downgrade(req) => {
                    handlers.with_mut(env, |handlers| {
                        handlers.install_focus(req);
                    });
                }
                CursorInteractionEventResponse::End => {
                    handlers.with_mut(env, |handlers| {
                        handlers.remove_cursor();
                    });
                }
            }
        }
        Some(DynInteractionHandler::Focus(handler)) => match handler.general_event(env, event) {
            FocusInteractionEventResponse::Keep => {}
            FocusInteractionEventResponse::Replace(req) => {
                handlers.with_mut(env, |handlers| {
                    handlers.install_focus(req);
                });
            }
            FocusInteractionEventResponse::End => {
                handlers.with_mut(env, |handlers| {
                    handlers.remove(FocusScope::KEYBOARD);
                });
            }
        },
        None => {}
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
