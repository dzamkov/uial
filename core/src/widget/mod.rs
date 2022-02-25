mod fill;
mod with_sizing;

use crate::*;
pub use fill::*;
use fortify::{Fortify, Lower};
use std::borrow::Cow;
pub use with_sizing::*;

/// A static description of a rectangular GUI element whose size and location has not yet been
/// determined. A widget may have size constraints and preferences that affect how it is used in
/// a layout.
pub trait Widget<S: State, D: Drawer> {
    /// The type of [`Element`] constructed by placing this [`Widget`].
    type Elem<'a, P: Placement<State = S>>: Element<D, State = S>
    where
        Self: 'a;

    /// Gets the sizing constraints and preferences for this [`Widget`] at the given state.
    fn sizing<'a>(&'a self, s: &'a S) -> Cow<'a, Sizing<i32>>;

    /// Specifies a concrete size and location for this [`Widget`], yielding an [`Element`].
    fn place<'a, P: Placement<State = S>>(&'a self, s: &mut S, placement: P) -> Self::Elem<'a, P>;
}

/// Specifies the size and location of a [`Widget`] and provides a means of interacting with other
/// related UI elements.
pub trait Placement {
    /// The type of application state this [`Placement`] interacts with.
    type State: State;

    /// Gets the current placement rectangle for this [`Placement`].
    fn rect(&self, s: &Self::State) -> Box2<i32>;
}

/// A GUI element which occupies a specific area on a two-dimensional surface. The size and
/// location of the element are determined upon creation, but can vary depending on the
/// application [`State`].
pub trait Element<D: Drawer> {
    /// The type of application state this [`Element`] interacts with.
    type State: State;

    /// Draws the element to `drawer`.
    fn draw_to(&self, s: &Self::State, drawer: &mut D);
}

impl<S: State, D: Drawer, T: Widget<S, D>> Widget<S, D> for Fortify<T>
where
    for<'a> T: Lower<'a>,
    for<'a> <T as Lower<'a>>::Target: Widget<S, D>,
{
    type Elem<'a, P: Placement<State = S>>
    where
        Self: 'a,
    = <<T as Lower<'a>>::Target as Widget<S, D>>::Elem<'a, P>;

    fn sizing<'a>(&'a self, s: &'a S) -> Cow<'a, Sizing<i32>> {
        self.borrow().sizing(s)
    }

    fn place<'a, P: Placement<State = S>>(&'a self, s: &mut S, placement: P) -> Self::Elem<'a, P> {
        self.borrow().place(s, placement)
    }
}