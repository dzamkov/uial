mod canvas;
mod dynamic;
mod empty;
mod extend;
mod fill;
mod on_click;
mod on_key;
mod overlay;
mod pad;
mod restrict;
mod stack;
mod switch;
mod label;
mod zoom_canvas;

use super::*;
pub use canvas::*;
pub use dynamic::*;
pub use empty::*;
pub use extend::*;
pub use fill::*;
pub use on_click::*;
pub use on_key::*;
pub use overlay::*;
pub use pad::*;
pub use restrict::*;
pub use stack::*;
use std::rc::Rc;
pub use switch::*;
pub use label::*;
pub use zoom_canvas::*;
use std::any::Any;

/// A trait which all [`Widget`]s must implement.
///
/// This is used primarily for defining functions and methods which compose [`Widget`]s, without
/// regard to their specific [`WidgetEnvironment`].
pub trait WidgetBase {}

/// Describes an interactive UI component that is displayed within a rectangle in discrete
/// two-dimensional space. `Env` encapsulates the external data and/or resources that the
/// widget has access to.
pub trait Widget<Env: WidgetEnvironment + ?Sized>: WidgetBase {
    /// Gets the sizing preferences/constraints for this [`Widget`].
    fn sizing(&self, env: &Env) -> Sizing;

    // TODO: Add `env` parameter once precise capturing has been implemented in rust.
    /// Creates an instance of this [`Widget`] within the given slot.
    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'a
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

/// Represents a location within a UI layout where a [`Widget`] may be instantiated.
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

/// A [`Widget`] which has been instantiated into a particular place within a UI layout.
pub trait WidgetInst<Env: WidgetEnvironment + ?Sized> {
    /// Draws this [`WidgetInst`] to the given drawer.
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer);

    /// Gets the interactable descendant of this [`WidgetInst`] at the given position, or [`None`]
    /// if no such widget exists.
    /// 
    /// This will typically be the widget that handles cursor events at the given position.
    fn identify(&self, env: &Env, pos: Vector2i) -> Option<WidgetId>;

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
    fn into_rc_dyn<'a>(self) -> Rc<dyn WidgetInst<Env> + 'a>
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

impl<T: WidgetBase + ?Sized> WidgetBase for Rc<T> {}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env> + ?Sized> Widget<Env> for Rc<T> {
    fn sizing(&self, env: &Env) -> Sizing {
        (**self).sizing(env)
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        (**self).inst(env, slot)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: WidgetInst<Env> + ?Sized> WidgetInst<Env> for Rc<T> {
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        (**self).draw(env, drawer)
    }

    fn identify(&self, env: &Env, pos: Vector2i) -> Option<WidgetId> {
        (**self).identify(env, pos)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WidgetId(unique::Unique);

impl WidgetId {
    /// Constructs a new [`WidgetId`], different from all other identifiers generated so far.
    pub fn new() -> Self {
        Self(unique::Unique::new())
    }
}

impl Default for WidgetId {
    fn default() -> Self {
        Self::new()
    }
}