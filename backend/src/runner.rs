use crate::wgpu::*;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;
use std::sync::Arc;
use uial::drawer::*;
use uial::prelude::*;

/// Manages the graphics context and windows that are used to display [`Widget`]s.
pub struct Runner<'a, S: HasReact + Track> {
    wgpu: Option<WgpuEnvironment>,
    keys_held: ReactCell<S::React, BTreeSet<KeyCode>>,
    windows: HashMap<winit::window::WindowId, RunnerWindow<'a, S>>,
}

/// Contains all `wgpu`-specific resources for a [`Runner`].
struct WgpuEnvironment {
    context: Arc<WgpuContext>,
    image_atlas: Arc<WgpuImageAtlas>,
    drawer_context: Arc<WgpuDrawerContext>,
}

/// Represents a window managed by a [`Runner`].
struct RunnerWindow<'a, S: HasReact + Track> {
    // NOTE: `cursor_handler` may contain references to `root_inst` that are hidden from the borrow
    // checker. It must be dropped before `root_inst`.
    cursor_handler: ReactCell<S::React, Option<RunCursorInteractionHandler<'static, S>>>,
    cursor_pos: ReactCell<S::React, Option<Point2i>>,
    // NOTE: `focus_handler` may contain references to `root_inst` that are hidden from the borrow
    // checker. It must be dropped before `root_inst`.
    focus_handler: ReactCell<S::React, Option<RunFocusInteractionHandler<'static, S>>>,
    // NOTE: `root_inst` may contain references to `root_widget` and `inner` that are hidden from
    // the borrow checker.  It must be dropped before they are dropped.
    root_inst: Rc<dyn WidgetInst<RunEnv<S>> + 'a>,
    root_widget: Rc<DynWidget<'a, RunEnv<S>>>,
    // NOTE: `surface` may contain references to `inner` that are hidden from the borrow checker.
    // It must be dropped before `inner` is dropped.
    surface: wgpu::Surface<'static>,
    config: RefCell<wgpu::SurfaceConfiguration>,
    drawer_resources: Cell<Option<WgpuDrawerResources>>,
    inner: Rc<RunnerWindowInner<S>>,
    close: Box<dyn FnMut(&mut S, &winit::event_loop::ActiveEventLoop) + 'a>,
}

/// A [`CursorInteractionHandler`] for a [`RunnerWindow`].
type RunCursorInteractionHandler<'ui, S> = Rc<dyn CursorInteractionHandler<'ui, RunEnv<S>> + 'ui>;

/// A [`FocusInteractionHandler`] for a [`RunnerWindow`].
type RunFocusInteractionHandler<'ui, S> = Rc<dyn FocusInteractionHandler<'ui, RunEnv<S>> + 'ui>;

/// Contains the data for a [`RunnerWindow`] which must be stored indirectly.
struct RunnerWindowInner<S: HasReact + Track> {
    /// The underlying [`winit::window::Window`].
    source: winit::window::Window,

    /// The size of the surface and widget for this window. This is tracked by the react system
    /// and will need to be kept in sync with the actual window size.
    size: ReactCell<S::React, Size2i>,
}

impl WgpuEnvironment {
    /// Constructs a new [`WgpuEnvironment`] based on the given [`WgpuContext`].
    pub fn new(context: WgpuContext, draw_format: wgpu::TextureFormat) -> Self {
        let context = Arc::new(context);
        let image_atlas = WgpuImageAtlas::new_arc(context.clone());
        let drawer_context = Arc::new(WgpuDrawerContext::new(
            context.clone(),
            image_atlas.clone(),
            draw_format,
        ));
        Self {
            context,
            image_atlas,
            drawer_context,
        }
    }
}

impl<'a, S: HasReact + Track> Runner<'a, S> {
    /// Creates a new [`Runner`].
    pub fn new(state: &mut S) -> Self {
        Self {
            wgpu: None,
            keys_held: state.react().new_cell(BTreeSet::new()),
            windows: HashMap::new(),
        }
    }

    /// Creates a window to be managed by this [`Runner`].
    ///
    /// The window will display the given widget and allow the user to interact with it.
    pub fn create_window(
        &mut self,
        state: &mut S,
        event_loop: &winit::event_loop::ActiveEventLoop,
        attrs: winit::window::WindowAttributes,
        widget: &dyn Fn(&RunEnv<S>) -> Rc<DynWidget<'a, RunEnv<S>>>,
        close: Box<dyn FnMut(&mut S, &winit::event_loop::ActiveEventLoop) + 'a>,
    ) -> Result<&winit::window::Window, winit::error::OsError> {
        // Create window
        let window = event_loop.create_window(attrs)?;
        let inner = Rc::new(RunnerWindowInner {
            source: window,
            size: state.react().new_cell(size2i(0, 0)),
        });
        let window = &inner.source;

        // Create `wgpu` resources if needed
        let (surface, drawer_context) = if let Some(wgpu) = &self.wgpu {
            todo!()
        } else {
            let inst = wgpu::Instance::new(&Default::default());
            let surface = inst.create_surface(window).unwrap();
            self.wgpu = Some(pollster::block_on(async {
                let adapter = inst
                    .request_adapter(&wgpu::RequestAdapterOptions {
                        compatible_surface: Some(&surface),
                        ..Default::default()
                    })
                    .await
                    .expect("failed to find an appropriate adapter");
                let (device, queue) = adapter
                    .request_device(
                        &wgpu::DeviceDescriptor {
                            label: None,
                            required_features: wgpu::Features::empty(),
                            required_limits: wgpu::Limits::default(),
                            memory_hints: wgpu::MemoryHints::default(),
                        },
                        None,
                    )
                    .await
                    .expect("failed to create device");
                let surface_capabilities = surface.get_capabilities(&adapter);
                let draw_format = surface_capabilities.formats[0];
                WgpuEnvironment::new(WgpuContext { device, queue }, draw_format)
            }));
            let wgpu = self.wgpu.as_ref().unwrap();
            (surface, &wgpu.drawer_context)
        };

        // Initialize widget
        let raw_env = RawRunEnv {
            state: &*state,
            runner: &*self,
        };
        let env = RunEnv::from_raw_ref(&raw_env);
        let root_widget = widget(env);
        let sizing = root_widget.sizing(env);

        // Apply min/max window size
        let size = window.inner_size();
        let mut size = size2i(size.width, size.height);
        if let Some((min, max)) = sizing.as_range() {
            size.x = u32::clamp(size.x, min.x, max.x);
            size.y = u32::clamp(size.y, min.y, max.y);
            let _ = window.request_inner_size(winit::dpi::PhysicalSize::new(size.x, size.y));
            window.set_min_inner_size(Some(winit::dpi::PhysicalSize::new(min.x, min.y)));
            window.set_max_inner_size(if max.x < u32::MAX && max.y < u32::MAX {
                Some(winit::dpi::PhysicalSize::new(max.x, max.y))
            } else {
                None
            });
        }

        // Instantiate widget
        let mut raw_env = RawRunEnv {
            state: &mut *state,
            runner: &*self,
        };
        let env = RunEnv::from_raw_mut(&mut raw_env);
        inner.size.set(env, size);
        let root_inst = Rc::new(root_widget.inst(env, inner.clone()));
        let root_inst: Rc<dyn WidgetInst<RunEnv<S>> + '_> = root_inst;

        // Configure surface
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: drawer_context.draw_format(),
            width: size.x,
            height: size.y,
            present_mode: wgpu::PresentMode::Fifo,
            desired_maximum_frame_latency: 2,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
        };
        surface.configure(&drawer_context.context().device, &config);

        // Extend the lifetimes for references within the `RunnerWindow`
        let surface: wgpu::Surface<'static> = unsafe { std::mem::transmute(surface) };
        let root_inst: Rc<dyn WidgetInst<RunEnv<S>>> = unsafe { std::mem::transmute(root_inst) };

        // Add window to map
        Ok(&self
            .windows
            .entry(window.id())
            .or_insert(RunnerWindow {
                cursor_handler: state.react().new_cell(None),
                cursor_pos: state.react().new_cell(None),
                focus_handler: state.react().new_cell(None),
                root_inst,
                root_widget,
                surface,
                config: RefCell::new(config),
                drawer_resources: Cell::new(None),
                inner,
                close,
            })
            .inner
            .source)
    }

    /// Forwards a window event to the [`Runner`].
    ///
    /// Returns `true` iff the event is for a window that is managed by this [`Runner`].
    pub fn window_event(
        &mut self,
        state: &mut S,
        event_loop: &winit::event_loop::ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: &winit::event::WindowEvent,
    ) -> bool {
        let _ = event_loop;
        let Some(window) = self.windows.get(&window_id) else {
            return false;
        };
        match event {
            winit::event::WindowEvent::Resized(n_size) => {
                if n_size.width > 0 && n_size.height > 0 {
                    let mut config = window.config.borrow_mut();
                    config.width = n_size.width;
                    config.height = n_size.height;
                    window
                        .inner
                        .size
                        .set(state, size2i(n_size.width, n_size.height));
                    window
                        .surface
                        .configure(&self.wgpu.as_ref().unwrap().context.device, &config);
                }
            }
            winit::event::WindowEvent::CloseRequested => {
                let id = window.inner.source.id();
                let mut window = self.windows.remove(&id).unwrap();
                (window.close)(state, event_loop);
            }
            winit::event::WindowEvent::Destroyed => todo!(),
            winit::event::WindowEvent::Focused(focused) => {
                if *focused {
                    if let Some(req) = window.root_inst.focus(
                        RunEnv::from_raw_mut(&mut RawRunEnv {
                            state,
                            runner: &*self,
                        }),
                        false,
                    ) {
                        let handler = req.handler;
                        let handler: RunFocusInteractionHandler<'static, S> =
                            unsafe { std::mem::transmute(handler) };
                        window.focus_handler.set(state, Some(handler));
                    }
                } else {
                    window.focus_handler.set(state, None);
                }
            }
            winit::event::WindowEvent::KeyboardInput {
                device_id: _,
                event:
                    winit::event::KeyEvent {
                        state: s,
                        physical_key: winit::keyboard::PhysicalKey::Code(key_code),
                        ..
                    },
                is_synthetic: _,
            } => self.general_event(
                window,
                state,
                GeneralEvent::Key {
                    key: Key {
                        key_code: Some(*key_code),
                    },
                    is_down: *s == winit::event::ElementState::Pressed,
                },
            ),
            winit::event::WindowEvent::CursorMoved {
                device_id: _,
                position,
            } => {
                window.cursor_pos.set(
                    state,
                    Some(vec2i(
                        position.x as i32,
                        window.inner.size.get(state).y as i32 - position.y as i32,
                    )),
                );
            }
            winit::event::WindowEvent::CursorLeft { device_id: _ } => {
                window.cursor_pos.set(state, None);
            }
            winit::event::WindowEvent::MouseWheel {
                device_id: _,
                delta,
                phase: _,
            } => self.cursor_event(window, state, CursorEvent::MouseScroll(*delta)),
            winit::event::WindowEvent::MouseInput {
                device_id: _,
                state: s,
                button,
            } => self.cursor_event(
                window,
                state,
                CursorEvent::MouseButton {
                    button: *button,
                    is_down: *s == winit::event::ElementState::Pressed,
                },
            ),
            winit::event::WindowEvent::RedrawRequested => {
                let wgpu = self.wgpu.as_ref().unwrap();
                let frame = match window.surface.get_current_texture() {
                    Ok(frame) => frame,
                    Err(wgpu::SurfaceError::Timeout) => window
                        .surface
                        .get_current_texture()
                        .expect("Failed to acquire next swap chain texture"),
                    Err(_) => {
                        let config = window.config.borrow();
                        window.surface.configure(&wgpu.context.device, &config);
                        window
                            .surface
                            .get_current_texture()
                            .expect("Failed to acquire next swap chain texture")
                    }
                };
                let view = frame
                    .texture
                    .create_view(&wgpu::TextureViewDescriptor::default());
                let size = window.inner.size.get(state);
                let mut resources = match window.drawer_resources.take() {
                    Some(resources) if resources.size() == size => resources,
                    _ => WgpuDrawerResources::new(&wgpu.drawer_context, size),
                };
                wgpu.drawer_context
                    .draw_to(&mut resources, &view, |drawer| {
                        window.root_inst.draw(
                            RunEnv::from_raw_ref(&RawRunEnv {
                                state,
                                runner: &*self,
                            }),
                            WgpuErasedDrawer::from_mut(drawer),
                        )
                    });
                window.drawer_resources.set(Some(resources));
                frame.present();
                window.inner.source.request_redraw();
            }
            _ => {}
        }
        true
    }

    /// Processes a [`CursorEvent`] for the given window.
    fn cursor_event(&self, window: &RunnerWindow<S>, state: &mut S, event: CursorEvent) {
        let handler = window.cursor_handler.get(state);
        if let Some(handler) = handler {
            if let Some(pos) = window.cursor_pos.get(state) {
                let res = handler.cursor_event(
                    RunEnv::from_raw_mut(&mut RawRunEnv {
                        state,
                        runner: self,
                    }),
                    pos,
                    event,
                );
                match res {
                    CursorInteractionEventResponse::Keep => {}
                    CursorInteractionEventResponse::Replace(req) => {
                        window.cursor_handler.set(state, Some(req.handler));
                    }
                    CursorInteractionEventResponse::Downgrade(req) => {
                        window.cursor_handler.set(state, None);
                        window.focus_handler.set(state, Some(req.handler));
                    }
                    CursorInteractionEventResponse::End => window.cursor_handler.set(state, None),
                }
            }
        } else if let Some(pos) = window.cursor_pos.get(state) {
            let res = window.root_inst.cursor_event(
                RunEnv::from_raw_mut(&mut RawRunEnv {
                    state,
                    runner: self,
                }),
                pos,
                event,
            );
            if let widget::CursorEventResponse::Start(req) = res {
                let handler: RunCursorInteractionHandler<S> = req.handler;
                let handler: RunCursorInteractionHandler<'static, S> =
                    unsafe { std::mem::transmute(handler) };
                window.cursor_handler.set(state, Some(handler));
            }
        }
    }

    /// Processes a [`GeneralEvent`] for the given window.
    fn general_event(&self, window: &RunnerWindow<S>, state: &mut S, event: GeneralEvent) {
        let focus_handler = window.focus_handler.get(state);
        if let Some(handler) = focus_handler {
            let res = handler.general_event(
                RunEnv::from_raw_mut(&mut RawRunEnv {
                    state,
                    runner: self,
                }),
                event,
            );
            match res {
                FocusInteractionEventResponse::Keep => {}
                FocusInteractionEventResponse::Replace(req) => {
                    window.focus_handler.set(state, Some(req.handler));
                }
                FocusInteractionEventResponse::End => {
                    window.focus_handler.set(state, None);
                }
            }
        }
    }

    pub fn device_event(
        &mut self,
        state: &mut S,
        event_loop: &winit::event_loop::ActiveEventLoop,
        device_id: winit::event::DeviceId,
        event: &winit::event::DeviceEvent,
    ) {
        let _ = (event_loop, device_id);
        #[allow(clippy::single_match)]
        match event {
            winit::event::DeviceEvent::Key(
                winit::event::RawKeyEvent {
                    physical_key: winit::keyboard::PhysicalKey::Code(key_code),
                    state: s
                }
            ) => {
                self.keys_held.with_mut(state, |held| {
                    if *s == winit::event::ElementState::Pressed {
                        held.insert(*key_code);
                    } else {
                        held.remove(key_code);
                    }
                });
            }
            _ => {}
        }
    }

    /// Iterates over the windows managed by this [`Runner`].
    pub fn windows(&self) -> impl Iterator<Item = &winit::window::Window> {
        self.windows.values().map(|w| &w.inner.source)
    }
}

/// The type of [`WidgetEnvironment`] provided by a [`Runner`].
#[repr(C)]
pub struct RunEnv<S: HasReact + Track> {
    raw: RawRunEnv<*mut S, *const Runner<'static, S>>,

    // Make `RunEnv` unsized to prevent swapping between mutable references, which would make
    // lifetime/mutability erasure unsound.
    _unsized: [()],
}

/// The internal data for a [`RunEnv`].
#[repr(C)]
struct RawRunEnv<State, Runner> {
    state: State,
    runner: Runner,
}

impl<S: HasReact + Track> RunEnv<S> {
    /// Converts a [`RawRunEnv`] reference into a [`RunEnv`] reference.
    fn from_raw_ref<'a>(raw: &'a RawRunEnv<&S, &Runner<S>>) -> &'a Self {
        unsafe { &*(core::ptr::slice_from_raw_parts(raw, 0) as *const RunEnv<S>) }
    }

    /// Converts a mutable [`RawRunEnv`] reference into a mutable [`RunEnv`] reference.
    fn from_raw_mut<'a>(raw: &'a mut RawRunEnv<&mut S, &Runner<S>>) -> &'a mut Self {
        unsafe { &mut *(core::ptr::slice_from_raw_parts_mut(raw, 0) as *mut RunEnv<S>) }
    }

    /// Gets a reference to the user-defined application state.
    pub fn state(&self) -> &S {
        unsafe { &*self.raw.state }
    }

    /// Gets a mutable reference to the user-defined application state.
    pub fn state_mut(&mut self) -> &mut S {
        unsafe { &mut *self.raw.state }
    }

    /// Gets a reference to the [`WgpuEnvironment`].
    fn wgpu(&self) -> &WgpuEnvironment {
        unsafe { &*self.raw.runner }.wgpu.as_ref().unwrap()
    }
}

impl<S: HasReact + Track> HasWgpuContext for RunEnv<S> {
    fn wgpu_context(&self) -> &Arc<WgpuContext> {
        &self.wgpu().context
    }
}

impl<S: HasReact + Track> HasWgpuDrawerContext for RunEnv<S> {
    fn wgpu_drawer_context(&self) -> &Arc<WgpuDrawerContext> {
        &self.wgpu().drawer_context
    }
}

impl<S: HasReact + Track> HasImageManager for RunEnv<S> {
    type ImageManager = Arc<WgpuImageAtlas>;
    fn image_manager(&self) -> &Self::ImageManager {
        &self.wgpu().image_atlas
    }
}

impl<S: HasReact + Track> HasReact for RunEnv<S> {
    type React = S::React;

    fn react(&self) -> &S::React {
        self.state().react()
    }

    fn react_mut(&mut self) -> &mut S::React {
        self.state_mut().react_mut()
    }
}

impl<S: HasReact + HasClock + Track> HasClock for RunEnv<S> {
    fn clock(&self) -> Instant {
        self.state().clock()
    }
}

impl<S: HasReact + Track> Track for RunEnv<S> {
    type ValidityToken = S::ValidityToken;

    fn track<R>(&self, inner: impl FnOnce() -> R) -> (R, Self::ValidityToken) {
        self.state().track(inner)
    }

    fn is_valid(&self, token: &Self::ValidityToken) -> bool {
        self.state().is_valid(token)
    }
}

impl<S: HasReact + Track> WidgetEnvironment for RunEnv<S> {
    type Drawer = WgpuErasedDrawer;

    fn is_key_down(&self, key: KeyCode) -> bool {
        let runner = unsafe { &*self.raw.runner };
        runner.keys_held.with_ref(self, |held| held.contains(&key))
    }

    fn interaction_feedback(&self, f: &mut dyn FnMut(&dyn std::any::Any)) {
        let runner = unsafe { &*self.raw.runner };
        for window in runner.windows.values() {
            let is_hovering = window.cursor_handler.with_ref(self, |h| {
                if let Some(h) = h {
                    h.feedback(self, f);
                    false
                } else {
                    true
                }
            });
            window.focus_handler.with_ref(self, |h| {
                if let Some(h) = h {
                    h.feedback(self, f)
                }
            });
            if is_hovering {
                if let Some(pos) = window.cursor_pos.get(self) {
                    window.root_inst.hover_feedback(self, pos, f);
                }
            }
        }
    }
}

impl<S: HasReact + Track> WidgetSlot<RunEnv<S>> for Rc<RunnerWindowInner<S>> {
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
