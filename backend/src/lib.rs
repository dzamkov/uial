#![allow(clippy::type_complexity)]
mod runner;
pub mod wgpu;

pub use runner::*;
use std::rc::Rc;
use uial::prelude::*;

/// The default state type for a simple application.
pub type DefaultState = (DefaultReact, Clock);

/// The default environment type for a simple application.
pub type DefaultEnv = RunEnv<DefaultState>;

/// Initializes and runs a [`SimpleApplication`].
pub fn run<App: SimpleApplication<DefaultEnv>>(init: impl FnOnce(&mut DefaultState) -> App) {
    let mut state = (
        unsafe { DefaultReact::new_static() },
        Clock::new(Duration::ZERO),
    );
    let app = init(&mut state);
    let runner = Runner::new(&mut state);
    let mut handler = SimpleAppHandler {
        start_inst: std::time::Instant::now(),
        state,
        runner,
        app: &app,
    };
    let event_loop = winit::event_loop::EventLoop::new().unwrap();
    event_loop.run_app(&mut handler).unwrap();
}

/// An application consisting of a single window.
pub trait SimpleApplication<Env: ?Sized + WidgetEnvironment> {
    /// Gets the title for the application window.
    fn title(&self, env: &Env) -> &str;

    /// Gets the [`Widget`] which is displayed in the body of the application window.
    fn body(&self, env: &Env) -> Rc<DynWidget<'_, Env>>;

    /// Updates the application in response to the passage of time.
    fn update(&self, env: &mut Env, delta_time: Duration) {
        // Nothing done by default
        let _ = (env, delta_time);
    }
}

/// A minimal implementation of [`SimpleApplication`].
pub struct SimpleApp<'a, Body> {
    /// The title of the application window.
    pub title: &'a str,

    /// The body of the application window.
    pub body: Body,
}

impl<Body: IntoWidget<DefaultEnv> + Clone> SimpleApplication<DefaultEnv> for SimpleApp<'_, Body> {
    fn title(&self, _: &DefaultEnv) -> &str {
        self.title
    }

    fn body(&self, env: &DefaultEnv) -> Rc<DynWidget<'_, DefaultEnv>> {
        widget::into_widget(&self.body, env).into_rc_dyn()
    }
}

/// The [`winit::application::ApplicationHandler`] for a [`SimpleApplication`].
struct SimpleAppHandler<'app, App> {
    start_inst: std::time::Instant,
    state: DefaultState,
    runner: Runner<'app, DefaultState>,
    app: &'app App,
}

impl<App: SimpleApplication<DefaultEnv>> SimpleAppHandler<'_, App> {
    /// Ensures `state` is up-to-date.
    fn update_state(&mut self) {
        let n_time = self.start_inst.elapsed().try_into().unwrap();
        let delta_time = n_time - self.state.1.get();

        // Update clock
        self.state.1.set(n_time);

        // Update application
        DefaultEnv::with_mut(&mut self.state, &self.runner, |env| {
            self.app.update(env, delta_time)
        })
    }
}

impl<App: SimpleApplication<DefaultEnv>> winit::application::ApplicationHandler
    for SimpleAppHandler<'_, App>
{
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        if self.runner.windows().next().is_none() {
            self.update_state();
            // TODO: Update title dynamically
            let title = DefaultEnv::with_ref(&self.state, &self.runner, |env| {
                self.app.title(env).to_owned()
            });
            self.runner
                .create_window(
                    &mut self.state,
                    event_loop,
                    winit::window::WindowAttributes::default().with_title(title),
                    &|env| self.app.body(env),
                    Box::new(|_, l| l.exit()),
                )
                .unwrap();
        }
    }

    fn window_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: winit::event::WindowEvent,
    ) {
        self.update_state();
        self.runner
            .window_event(&mut self.state, event_loop, window_id, &event);
    }

    fn device_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        device_id: winit::event::DeviceId,
        event: winit::event::DeviceEvent,
    ) {
        self.update_state();
        self.runner
            .device_event(&mut self.state, event_loop, device_id, &event);
    }
}
