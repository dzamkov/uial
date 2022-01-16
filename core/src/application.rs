use crate::*;

/// An interface for setting up and running an application.
pub trait Application<'a> {
    /// The type of the [`State`] of this application.
    type State: State + 'a;

    /// The type of [`Drawer`] used for this application.
    type Drawer: Drawer + 'a;

    /// The current state of the application.
    fn state(&mut self) -> &mut Self::State;

    /// Runs a [`Widget`].
    fn run(self, widget: impl Widget<Self::State, Self::Drawer> + 'a) -> !;
}