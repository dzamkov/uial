mod fill;
mod image;
mod prepare;
mod with_padding;
mod with_sizing;

pub use self::image::*;
use crate::*;
pub use fill::*;
use fortify::*;
pub use prepare::*;
use std::borrow::Cow;
pub use with_padding::*;
pub use with_sizing::*;

/// A static description of a rectangular GUI element whose size and location has not yet been
/// determined. A widget may have size constraints and preferences that affect how it is used in
/// a layout.
pub trait Widget: for<'a> WidgetInst<'a> {}

/// Encapsulates the basic information for a [`Widget`] in any phase of use.
pub trait WidgetBase {
    /// The type of [`State`] the widget interacts with.
    type State: State;

    /// The type of graphics context used by the widget.
    type Graphics: Graphics;
}

/// A [`Widget`] which can be instantiated in a context of lifetime `'a`.
pub trait WidgetInst<'a>: WidgetBase {
    /// The type of [`WidgetInst`] that results from instantiating this widget.
    type Inst: WidgetPlace<State = Self::State, Graphics = Self::Graphics>;

    /// Instantiates this [`Widget`], providing it with the specific [`State`] and [`Graphics`] it
    /// will be using. This is an opportunity for the [`Widget`] to initialize state and
    /// prepare graphics resources.
    fn inst(
        self,
        s: &mut Self::State,
        g: &'a Self::Graphics,
    ) -> (Self::Inst, <Self::Inst as WidgetPlace>::Key);
}

/// A [`Widget`] that is ready to be placed.
pub trait WidgetPlace: WidgetBase {
    /// The information needed to place this [`Widget`]. Typically, there can be at most one
    /// `Key` per widget, preventing it from being placed multiple times.
    type Key;

    /// The type of [`Element`] constructed by placing the [`Widget`].
    type Elem<'a, P: Placement<State = Self::State>>: Element<Self::Graphics, State = Self::State>
    where
        Self: 'a;

    /// Gets the sizing constraints and preferences for the [`Widget`] at the given state.
    fn sizing<'a>(&'a self, s: &'a Self::State) -> Cow<'a, Sizing>;

    /// Specifies a concrete size and location for the [`Widget`], yielding an [`Element`].
    fn place<'a, P: Placement<State = Self::State>>(
        &'a self,
        s: &mut Self::State,
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

impl<T: for<'a> WidgetInst<'a>> Widget for T {}

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