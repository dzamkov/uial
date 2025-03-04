use crate::prelude::*;

/// An [`IntoWidget`] defined by a function or closure.
#[derive(Clone, Copy)]
pub struct Build<F> {
    pub build: F,
}

impl<F> Build<F> {
    /// Constructs a new [`Build`] widget.
    pub fn new(build: F) -> Self {
        Self { build }
    }
}

/// Constructs a new [`Build`] widget.
pub fn build<F>(build: F) -> Build<F> {
    Build::new(build)
}

impl<F> WidgetLike for Build<F> {}

impl<Env: WidgetEnvironment + ?Sized, F: FnOnce(&Env) -> T, T: IntoWidget<Env>> IntoWidget<Env>
    for Build<F>
{
    fn into_widget(self, env: &Env) -> impl Widget<Env> {
        (self.build)(env).into_widget(env)
    }
}