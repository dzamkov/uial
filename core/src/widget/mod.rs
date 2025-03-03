mod canvas;
mod dynamic;
mod empty;
mod extend;
mod fill;
mod label;
mod minimize;
mod on_click;
mod on_key;
mod overlay;
mod pad;
mod restrict;
mod stack;
mod switch;
mod zoom_canvas;

use crate::prelude::*;
use crate::unique::Unique;
pub use canvas::*;
pub use dynamic::*;
pub use empty::*;
pub use extend::*;
pub use fill::*;
pub use label::*;
pub use minimize::*;
pub use on_click::*;
pub use on_key::*;
pub use overlay::*;
pub use pad::*;
pub use restrict::*;
pub use stack::*;
use std::any::Any;
use std::rc::Rc;
pub use switch::*;
pub use zoom_canvas::*;

/// A widget-like object to which widget decorators and composition operators can be applied.
///
/// All [`Widget`]s and [`IntoWidget`]s, for any [`WidgetEnvironment`], are [`WidgetLike`].
pub trait WidgetLike {}

/// Can be converted into a [`Widget`].
///
/// This is trivially implemented for all [`Widget`]s, but may also be implemented for static
/// descriptions of a widget (i.e. a widget builder), and references to them. In the latter
/// each call to [`IntoWidget::into_widget`] should yield a new instance of the widget which
/// is entirely independent of the previous instances.
///
/// There is no blanket implementation for [`Widget`] because some decorators and composition
/// operators should implement [`IntoWidget`] manually.
pub trait IntoWidget<Env: WidgetEnvironment + ?Sized>: WidgetLike {
    /// Converts this [`IntoWidget`] into a [`Widget`].
    // TODO: The return type should not capture the lifetime associated with `env`, but there
    // is no way to express this for now.
    // Fix once https://github.com/rust-lang/rust/issues/130044 is stablized
    fn into_widget(self, env: &Env) -> impl Widget<Env>
    where
        Self: Sized;
}

/// Shortcut for [`IntoWidget::into_widget`].
// TODO: This is a hack to work around https://github.com/rust-lang/rust/issues/130044
// once this is stablized, we can remove the entire function
pub fn into_widget<Env: WidgetEnvironment + ?Sized + 'static, T: IntoWidget<Env>>(
    widget: T,
    env: &Env,
) -> impl Widget<Env> + use<T, Env> {
    let env: &'static Env = unsafe { std::mem::transmute(env) };
    widget.into_widget(env)
}

/// Describes an interactive UI component that can be displayed within a rectangle in discrete
/// two-dimensional space.
///
/// When a [`Widget`] is [`Clone`], cloning it will yield an identical [`Widget`] with shared
/// internal state and identity. e.g. a cloned button will appear to be highlighted and pressed
/// when the original button is highlighted and pressed. This is occasionally useful when there
/// is a need to define different views/layouts of the same set of widgets. However, in the case
/// where multiple independent widgets are desired, you should instead make multiple calls to
/// [`IntoWidget::into_widget`] of a static description of the widget.
///
/// `Env` encapsulates the external data and/or resources that the widget has access to.
pub trait Widget<Env: WidgetEnvironment + ?Sized>: IntoWidget<Env> {
    /// Gets the sizing preferences/constraints for this [`Widget`].
    fn sizing(&self, env: &Env) -> Sizing;

    /// Creates an instance of this [`Widget`] placed in the given slot.
    fn place<'a, S: WidgetSlot<Env> + 'a>(
        &'a self,
        env: &Env,
        slot: S,
    ) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a;

    /// Converts this [`Widget`] into a [`Rc`]-wrapped [`DynWidget`].
    fn into_rc_dyn<'a>(self) -> Rc<DynWidget<'a, Env>>
    where
        Self: Sized + 'a,
    {
        DynWidget::from_rc(Rc::new(self))
    }
}

/// Represents a location within a UI layout where a [`Widget`] may be placed.
pub trait WidgetSlot<Env: WidgetEnvironment + ?Sized>: Clone {
    /// Indicates whether the widget in this slot is part of the current layout.
    fn is_visible(&self, env: &Env) -> bool;

    /// The size allocated for the widget in this slot.
    fn size(&self, env: &Env) -> Size2i;

    /// The location of the minimum corner of this slot.
    fn min(&self, env: &Env) -> Point2i;

    /// The bounds of this slot.
    fn bounds(&self, env: &Env) -> Box2i {
        Box2i::from_min_size(self.min(env), self.size(env))
    }

    /// Called when a general event is "bubbled" up from the widget in this slot.
    ///
    /// Note that this will only happen if there is an active focus interaction for one of
    /// the descendants of this slot.
    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent);
}

/// Encapsulates the functionality of a [`Widget`] when it is "placed" into a particular
/// [`WidgetSlot`].
pub trait WidgetPlaced<Env: WidgetEnvironment + ?Sized> {
    /// Draws this [`WidgetInst`] to the given drawer.
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer);

    /// Calls `f` for each feedback item produced by the "hover" interaction resulting from a
    /// cursor hovering at the given position.
    ///
    /// Feedback items are typically related to the descendant widget that would handle cursor
    /// events at the given position. This returns `true` iff such cursor events would be handled,
    /// as opposed to being "bubbled" up to a parent widget.
    ///
    /// Interaction feedback can be accessed by calling [`WidgetEnvironment::interaction_feedback`].
    fn hover_feedback(&self, env: &Env, pos: Vector2i, f: &mut dyn FnMut(&dyn Any)) -> bool;

    /// Processes an "initial" [`CursorEvent`] that may be within the bounds of this [`WidgetInst`]
    /// while there is no ongoing interaction involving the cursor.
    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env>;

    /// Called when focus enters this [`WidgetInst`].
    ///
    /// The widget may request a focus interaction to begin so that it may capture future input.
    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>>;

    /// Converts this [`WidgetInst`] into a [`Rc`]-wrapped dynamic [`WidgetInst`].
    fn into_rc_dyn<'a>(self) -> Rc<dyn WidgetPlaced<Env> + 'a>
    where
        Self: Sized + 'a,
    {
        Rc::new(self)
    }
}

/// A possible response to an initial cursor event on a [`WidgetInst`].
pub enum CursorEventResponse<'ui, Env: WidgetEnvironment + ?Sized> {
    /// The event was handled without starting a new interaction.
    Handled,

    /// The event was handled, and should begin a new interaction.
    Start(CursorInteractionRequest<'ui, Env>),

    /// The event was not handled.
    Bubble,
}

/// A valid environment for a [`Widget`]. This encapsulates the external data and/or resources that
/// the widget has access to.
pub trait WidgetEnvironment {
    /// The type of drawing interface provided to [`Widget`]s to draw themselves in this
    /// environment.
    type Drawer: ?Sized;

    /// Determines whether the given keyboard key is currently pressed.
    fn is_key_down(&self, key: KeyCode) -> bool;

    /// Calls `f` for every feedback item for all active interactions.
    fn interaction_feedback(&self, f: &mut dyn FnMut(&dyn Any));
}

impl<T: WidgetLike + ?Sized> WidgetLike for Rc<T> {}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env> + ?Sized> IntoWidget<Env> for Rc<T> {
    fn into_widget(self, _: &Env) -> impl Widget<Env> {
        self.clone()
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env> + ?Sized> Widget<Env> for Rc<T> {
    fn sizing(&self, env: &Env) -> Sizing {
        (**self).sizing(env)
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(
        &'a self,
        env: &Env,
        slot: S,
    ) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        (**self).place(env, slot)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: WidgetPlaced<Env> + ?Sized> WidgetPlaced<Env> for Rc<T> {
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        (**self).draw(env, drawer)
    }

    fn hover_feedback(&self, env: &Env, pos: Vector2i, f: &mut dyn FnMut(&dyn Any)) -> bool {
        (**self).hover_feedback(env, pos, f)
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env> {
        (**self).cursor_event(env, pos, event)
    }

    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>> {
        (**self).focus(env, backward)
    }
}

/// A unique identifier for a [`Widget`].
///
/// Only certain widgets will have an [`WidgetId`], usually when it is necessary to identify it
/// within a [`WidgetEnvironment`].
///
/// Cloning a [`Widget`] will not create a new [`WidgetId`]. However, calling
/// [`IntoWidget::into_widget`] on a static description of a widget will.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WidgetId(Unique);

impl WidgetId {
    /// Constructs a new [`WidgetId`], different from all other identifiers generated so far.
    pub fn new() -> Self {
        Self(Unique::new())
    }
}

impl Default for WidgetId {
    fn default() -> Self {
        Self::new()
    }
}
