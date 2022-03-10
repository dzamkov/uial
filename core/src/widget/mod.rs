mod fill;
mod prepare;
mod with_padding;
mod with_sizing;

use crate::*;
pub use fill::*;
use fortify::Lower;
pub use prepare::*;
use std::borrow::Cow;
pub use with_padding::*;
pub use with_sizing::*;

/// A static description of a rectangular GUI element whose size and location has not yet been
/// determined. A widget may have size constraints and preferences that affect how it is used in
/// a layout.
pub trait Widget<S: State, G: Graphics>: WidgetBase {
    /// The type of [`WidgetInst`] that results from instantiating this widget.
    type Inst<'a>: WidgetInst<S, G>
    where
        G: 'a;

    /// Instantiates this [`Widget`], providing it with the specific [`State`] and [`Graphics`] it
    /// will be using. This is an opportunity for the [`Widget`] to initialize state and
    /// prepare graphics resources.
    fn inst<'a>(
        self,
        s: &mut S,
        g: &'a G,
    ) -> (Self::Inst<'a>, <Self::Inst<'a> as WidgetInst<S, G>>::Key);
}

/// A widget of some [`State`] and [`Graphics`] type. This is used to defined [`Widget`] extension
/// methods.
pub trait WidgetBase {}

/// The functionality of [`WidgetInst`] that is independent of graphics context.
pub trait WidgetInstBase<S: State> {
    /// Gets the sizing constraints and preferences for the [`Widget`] at the given state.
    fn sizing<'a>(&'a self, s: &'a S) -> Cow<'a, Sizing>;
}

/// A [`Widget`] that has been instantiated with a [`State`] and [`Graphics`], and is now
/// ready to be placed.
pub trait WidgetInst<S: State, G: Graphics>: WidgetInstBase<S> {
    /// The information needed to place this [`Widget`]. Typically, there can be at most one
    /// `Key` per widget, preventing it from being placed multiple times.
    type Key;

    /// The type of [`Element`] constructed by placing the [`Widget`].
    type Elem<'a, P: Placement<State = S>>: Element<G, State = S>
    where
        Self: 'a;

    /// Specifies a concrete size and location for the [`Widget`], yielding an [`Element`].
    fn place<'a, P: Placement<State = S>>(
        &'a self,
        s: &mut S,
        key: Self::Key,
        placement: P,
    ) -> Self::Elem<'a, P>;
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
pub trait Element<G: Graphics> {
    /// The type of application state this [`Element`] interacts with.
    type State: State;

    /// Draws the element to `drawer`.
    fn draw_to(&self, s: &Self::State, drawer: &mut G::Drawer<'_>);
}

unsafe impl<'a, 'b, S: State + 'b, G: Graphics + 'b> Lower<'b> for dyn Element<G, State = S> + 'a {
    type Target = dyn Element<G, State = S> + 'b;
    fn lower_ref<'c>(&'c self) -> &'c Self::Target
    where
        Self: 'b,
        'b: 'c,
    {
        self
    }
}
