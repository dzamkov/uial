mod dynamic;

use super::WidgetEnvironment;
use super::downcast::DowncastRef;
pub use dynamic::*;
use std::rc::Rc;
use uial_geometry::Point2i;
pub use winit::event::VirtualKeyCode;

/// Represents a mouse pointer or similar input device.
pub trait Cursor<'cursor, Env: WidgetEnvironment + ?Sized>: 'cursor {
    /// Gets the current position of the cursor.
    fn pos(&self, env: &Env) -> Point2i;

    /// The type of [`Keyboard`] associated with this cursor.
    type Keyboard: Keyboard<'cursor, Env>;

    /// Gets the [`Keyboard`] associated with this cursor.
    fn keyboard(&self, env: &Env) -> Option<Self::Keyboard>;

    /// Installs a [`CursorHandler`] to capture all future cursor events for the cursor.
    fn set_handler(&self, env: &mut Env, handler: impl CursorHandler<Env> + 'cursor);

    /// Resets the [`CursorHandler`] for this cursor to its default.
    fn clear_handler(&self, env: &mut Env);

    /// Calls the given function for each [`Interaction`] that arises from this cursor hovering
    /// over a widget.
    fn default_interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) -> EventStatus;

    /// Calls the default event handler for a mouse scroll event.
    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus;

    /// Calls the default event handler for a mouse down event.
    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus;

    /// Converts this [`Cursor`] into an [`Rc`]-wrapped [`DynCursor`].
    fn into_rc_dyn(self) -> Rc<DynCursor<'cursor, Env>>
    where
        Self: Sized,
    {
        DynCursor::from_rc(Rc::new(self))
    }
}

/// An event handler that can be attached to a [`Cursor`] to capture cursor events.
pub trait CursorHandler<Env: WidgetEnvironment + ?Sized> {
    /// Calls the given function for each [`Interaction`] this cursor is participating in.
    fn interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction));

    /// Responds to a mouse scroll on the associated cursor.
    fn mouse_scroll(&self, env: &mut Env, amount: ScrollAmount);

    /// Responds to a mouse button being pressed on the associated cursor.
    fn mouse_down(&self, env: &mut Env, button: MouseButton);

    /// Responds to a mouse button being released on the associated cursor.
    fn mouse_up(&self, env: &mut Env, button: MouseButton) {
        // Not handled by default
        let _ = (env, button);
    }
}

/// Represents a keyboard input device.
pub trait Keyboard<'keyboard, Env: WidgetEnvironment + ?Sized>: 'keyboard {
    /// Calls the given function for each key currently held down on the keyboard.
    fn keys_held(&self, env: &Env, held: impl FnMut(Key));

    /// Installs a different [`Keyboard`] to capture all future keyboard events.
    fn set_handler(&self, env: &mut Env, handler: impl KeyboardHandler<Env> + 'keyboard);

    /// Calls the default event handler for a key down event.
    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus;

    /// Converts this [`Keyboard`] into an [`Rc`]-wrapped [`DynKeyboard`].
    fn into_rc_dyn(self) -> Rc<DynKeyboard<'keyboard, Env>>
    where
        Self: Sized,
    {
        DynKeyboard::from_rc(Rc::new(self))
    }
}

/// An event handler that can be attached to a [`Keyboard`] to capture keyboard events.
pub trait KeyboardHandler<Env: WidgetEnvironment + ?Sized> {
    /// Responds to a keyboard key being pressed while this handler is active.
    fn key_down(&self, env: &mut Env, key: Key);

    /// Responds to a keyboard key being released while this handler is active.
    fn key_up(&self, env: &mut Env, key: Key) {
        // Not handled by default
        let _ = (env, key);
    }
}

/// Provides a high-level description of an ongoing user interaction involving a [`Cursor`] or a
/// [`Keyboard`]. All such interactions can be queried from a [`WidgetEnvironment`] and used to
/// provide feedback for the interaction.
pub trait Interaction: std::any::Any {
    /// Attempts to downcast this [`Interaction`] to a specific type, putting a reference to it
    /// in the given [`DowncastRef`].
    fn downcast_ref_to<'a>(&'a self, target: DowncastRef<'_, 'a>);
}

/// Contains extension methods for [`Interaction`].
pub trait InteractionExt: Interaction {
    /// Attempts to downcast this [`Interaction`] to a specific type.
    fn downcast_ref<T: std::any::Any + ?Sized>(&self) -> Option<&T> {
        // TODO: Fast path when `self.type_id() == std::any::TypeId::of::<T>()`
        let mut target = None;
        self.downcast_ref_to((&mut target).into());
        target
    }
}

impl<T: Interaction + ?Sized> InteractionExt for T {}

impl<'cursor, Env: WidgetEnvironment + ?Sized, T: Cursor<'cursor, Env> + ?Sized>
    Cursor<'cursor, Env> for &'cursor T
{
    fn pos(&self, env: &Env) -> Point2i {
        (**self).pos(env)
    }

    type Keyboard = T::Keyboard;
    fn keyboard(&self, env: &Env) -> Option<Self::Keyboard> {
        (**self).keyboard(env)
    }

    fn set_handler(&self, env: &mut Env, handler: impl CursorHandler<Env> + 'cursor) {
        (**self).set_handler(env, handler)
    }

    fn clear_handler(&self, env: &mut Env) {
        (**self).clear_handler(env)
    }

    fn default_interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) -> EventStatus {
        (**self).default_interactions(env, f)
    }

    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus {
        (**self).default_mouse_scroll(env, amount)
    }

    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus {
        (**self).default_mouse_down(env, button)
    }
}

impl<'cursor, Env: WidgetEnvironment + ?Sized, T: Cursor<'cursor, Env>> Cursor<'cursor, Env>
    for Rc<T>
{
    fn pos(&self, env: &Env) -> Point2i {
        (**self).pos(env)
    }

    type Keyboard = T::Keyboard;
    fn keyboard(&self, env: &Env) -> Option<Self::Keyboard> {
        (**self).keyboard(env)
    }

    fn set_handler(&self, env: &mut Env, handler: impl CursorHandler<Env> + 'cursor) {
        (**self).set_handler(env, handler)
    }

    fn clear_handler(&self, env: &mut Env) {
        (**self).clear_handler(env)
    }

    fn default_interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) -> EventStatus {
        (**self).default_interactions(env, f)
    }

    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus {
        (**self).default_mouse_scroll(env, amount)
    }

    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus {
        (**self).default_mouse_down(env, button)
    }

    fn into_rc_dyn(self) -> Rc<DynCursor<'cursor, Env>> {
        DynCursor::from_rc(self)
    }
}

impl<'keyboard, Env: WidgetEnvironment + ?Sized, T: Keyboard<'keyboard, Env> + ?Sized>
    Keyboard<'keyboard, Env> for &'keyboard T
{
    fn keys_held(&self, env: &Env, held: impl FnMut(Key)) {
        (**self).keys_held(env, held)
    }

    fn set_handler(&self, env: &mut Env, handler: impl KeyboardHandler<Env> + 'keyboard) {
        (**self).set_handler(env, handler)
    }

    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus {
        (**self).default_key_down(env, key)
    }
}

impl<'keyboard, Env: WidgetEnvironment + ?Sized, T: Keyboard<'keyboard, Env>>
    Keyboard<'keyboard, Env> for Rc<T>
{
    fn keys_held(&self, env: &Env, held: impl FnMut(Key)) {
        (**self).keys_held(env, held)
    }

    fn set_handler(&self, env: &mut Env, handler: impl KeyboardHandler<Env> + 'keyboard) {
        (**self).set_handler(env, handler)
    }

    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus {
        (**self).default_key_down(env, key)
    }

    fn into_rc_dyn(self) -> Rc<DynKeyboard<'keyboard, Env>> {
        DynKeyboard::from_rc(self)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: CursorHandler<Env> + ?Sized> CursorHandler<Env>
    for Box<T>
{
    fn interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) {
        (**self).interactions(env, f)
    }

    fn mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) {
        (**self).mouse_scroll(env, amount)
    }

    fn mouse_down(&self, env: &mut Env, button: MouseButton) {
        (**self).mouse_down(env, button)
    }

    fn mouse_up(&self, env: &mut Env, button: MouseButton) {
        (**self).mouse_up(env, button)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: KeyboardHandler<Env> + ?Sized> KeyboardHandler<Env>
    for Box<T>
{
    fn key_down(&self, env: &mut Env, key: Key) {
        (**self).key_down(env, key)
    }

    fn key_up(&self, env: &mut Env, key: Key) {
        (**self).key_up(env, key)
    }
}

/// A [`CursorHandler`] or [`KeyboardHandler`] which defers to the default handler for the
/// associated [`Cursor`] or [`Keyboard`].
pub struct DefaultHandler<T>(pub T);

impl<'cursor, Env: WidgetEnvironment + ?Sized, T: Cursor<'cursor, Env>> CursorHandler<Env>
    for DefaultHandler<T>
{
    fn interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) {
        self.0.default_interactions(env, f);
    }

    fn mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) {
        self.0.default_mouse_scroll(env, amount);
    }

    fn mouse_down(&self, env: &mut Env, button: MouseButton) {
        self.0.default_mouse_down(env, button);
    }
}

impl<'keyboard, Env: WidgetEnvironment + ?Sized, T: Keyboard<'keyboard, Env>> KeyboardHandler<Env>
    for DefaultHandler<T>
{
    fn key_down(&self, env: &mut Env, key: Key) {
        self.0.default_key_down(env, key);
    }
}

impl<Env: WidgetEnvironment + ?Sized> CursorHandler<Env> for () {
    fn interactions(&self, _: &Env, _: impl FnMut(&dyn Interaction)) {
        // Nothing to do here
    }

    fn mouse_scroll(&self, _: &mut Env, _: ScrollAmount) {
        // Nothing to do here
    }

    fn mouse_down(&self, _: &mut Env, _: MouseButton) {
        // Nothing to do here
    }
}

impl<Env: WidgetEnvironment + ?Sized> KeyboardHandler<Env> for () {
    fn key_down(&self, _: &mut Env, _: Key) {
        // Nothing to do here
    }
}

/// Describes the status of an input event after a handler processes it.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum EventStatus {
    /// The event was not handled, and should be passed to the handler with the next lower
    /// priority.
    Bubble,

    /// The event was handled.
    Handled,
}

/// Identifies a button on a mouse.
pub type MouseButton = winit::event::MouseButton;

/// Identifies/describes a keyboard key.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Key {
    pub scan_code: winit::event::ScanCode,
    pub virtual_key_code: Option<winit::event::VirtualKeyCode>,
}

/// Describes an amount of scrolling performed by a mouse or similar input device.
#[derive(Debug, Clone, Copy)]
pub enum ScrollAmount {
    Ticks([f32; 2]),
    Pixels([f32; 2]),
}
