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

/// Runs an application consisting of a single window. Returns when the window is closed (if
/// the platform supports it).
pub fn run<'a>(title: &str, body: &dyn Fn(&DefaultEnv) -> Rc<DynWidget<'a, DefaultEnv>>) {
    let mut state = (
        unsafe { DefaultReact::new_static() },
        Clock::new(Duration::ZERO),
    );
    let runner = Runner::new(&mut state);
    let mut app = DefaultApp {
        start_inst: std::time::Instant::now(),
        state,
        runner,
        title,
        body,
    };
    let event_loop = winit::event_loop::EventLoop::new().unwrap();
    event_loop.run_app(&mut app).unwrap();
}

/// The [`winit::application::ApplicationHandler`] for a simple application.
struct DefaultApp<'a, 'b> {
    start_inst: std::time::Instant,
    state: DefaultState,
    runner: Runner<'b, DefaultState>,
    title: &'a str,
    body: &'a dyn Fn(&DefaultEnv) -> Rc<DynWidget<'b, DefaultEnv>>,
}

impl DefaultApp<'_, '_> {
    /// Ensures `state` is up-to-date.
    fn update_state(&mut self) {
        self.state
            .1
            .set(self.start_inst.elapsed().try_into().unwrap());
    }
}

impl winit::application::ApplicationHandler for DefaultApp<'_, '_> {
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        if self.runner.windows().next().is_none() {
            self.update_state();
            self.runner
                .create_window(
                    &mut self.state,
                    event_loop,
                    winit::window::WindowAttributes::default().with_title(self.title.to_owned()),
                    self.body,
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
