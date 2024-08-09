use super::*;
use std::rc::Rc;
use uial::prelude::{Clock, DefaultReact, Duration, DynWidget, Widget};

/// The state type for a [`SimpleApplication`].
pub type SimpleState = (DefaultReact, Clock);

/// An [`Application`] defined by a static title and body constructor.
pub struct SimpleApplication {
    pub title: &'static str,
    pub body: &'static dyn Fn(&RunEnv<SimpleState>) -> Rc<DynWidget<'static, RunEnv<SimpleState>>>,
}

impl Application for SimpleApplication {
    type State = SimpleState;

    fn title(&self) -> &str {
        self.title
    }

    fn body(&self, env: &RunEnv<SimpleState>) -> impl Widget<RunEnv<SimpleState>> + '_ {
        (self.body)(env)
    }

    fn update(&self, env: &mut RunEnv<Self::State>, delta_time: Duration) {
        let clock = &mut env.state_mut().1;
        clock.set(clock.get() + delta_time);
    }
}

impl SimpleApplication {
    /// Runs this [`SimpleApplication`].
    pub fn run(self) -> ! {
        let state = (
            unsafe { DefaultReact::new_static() },
            Clock::new(Duration::ZERO),
        );
        run(self, state)
    }
}
