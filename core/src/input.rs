use crate::prelude::*;
use std::any::Any;
use std::rc::Rc;

/// A widget-specific event handler for a cursor interaction.
pub trait CursorInteractionHandler<'ui, Env: WidgetEnvironment + ?Sized> {
    /// Indicates whether the cursor should be "locked" as part of this interaction. If so, it
    /// will be hidden and confined to the window.
    fn is_locked(&self, env: &Env) -> bool;

    /// Processes a cursor-specific event.
    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorInteractionEventResponse<'ui, Env>;

    /// Processes a general event.
    fn general_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: GeneralEvent,
    ) -> CursorInteractionEventResponse<'ui, Env>;

    /// Calls `f` for each feedback item produced by this interaction.
    ///
    /// Interaction feedback can be accessed by calling [`WidgetEnvironment::interaction_feedback`].
    fn feedback(&self, env: &Env, f: &mut dyn FnMut(&dyn Any));
}

/// A widget-specific event handler for a focus interaction.
pub trait FocusInteractionHandler<'ui, Env: WidgetEnvironment + ?Sized> {
    /// Processes a general event.
    fn general_event(
        &self,
        env: &mut Env,
        event: GeneralEvent,
    ) -> FocusInteractionEventResponse<'ui, Env>;

    /// Calls `f` for each feedback item produced by this interaction.
    ///
    /// Interaction feedback can be accessed by calling [`WidgetEnvironment::interaction_feedback`].
    fn feedback(&self, env: &Env, f: &mut dyn FnMut(&dyn Any)) {
        // No feedback by default
        let _ = (env, f);
    }
}

/// A possible response to a cursor interaction event.
pub enum CursorInteractionEventResponse<'ui, Env: WidgetEnvironment + ?Sized> {
    /// The interaction should continue.
    Keep,

    /// The interaction will be replaced by another interaction.
    Replace(CursorInteractionRequest<'ui, Env>),

    /// The interaction will be replaced by a focus interaction.
    Downgrade(FocusInteractionRequest<'ui, Env>),

    /// The interaction has ended.
    End,
}

/// A request to begin or switch to a new cursor interaction.
pub struct CursorInteractionRequest<'ui, Env: WidgetEnvironment + ?Sized> {
    /// The scope of the interaction. The request will remove all other interactions with
    /// overlapping scopes.
    pub scope: FocusScope,

    /// The handler to be installed for the interaction.
    pub handler: Rc<dyn CursorInteractionHandler<'ui, Env> + 'ui>,
}

/// A possible response to a focus interaction event.
pub enum FocusInteractionEventResponse<'ui, Env: WidgetEnvironment + ?Sized> {
    /// The interaction should continue.
    Keep,

    /// The interaction will be replaced by another interaction.
    Replace(FocusInteractionRequest<'ui, Env>),

    /// The interaction has ended.
    End,
}

/// A request to begin or switch to a new focus interaction.
pub struct FocusInteractionRequest<'ui, Env: WidgetEnvironment + ?Sized> {
    /// The scope of the interaction. The request will remove all other interactions with
    /// overlapping scopes.
    pub scope: FocusScope,

    /// The handler to be installed for the interaction.
    pub handler: Rc<dyn FocusInteractionHandler<'ui, Env> + 'ui>,
}

/// Describes the scope of a focus interaction.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct FocusScope {
    /// Indicates whether the focus interaction captures keyboard input.
    pub keyboard: bool,

    /// Indicates whether the focus interaction captures gamepad input.
    pub gamepad: bool,
}

impl FocusScope {
    /// A [`FocusScope`] which captures all available general input.
    pub const ALL: Self = Self {
        keyboard: true,
        gamepad: true,
    };

    /// A [`FocusScope`] which captures no input.
    pub const NONE: Self = Self {
        keyboard: false,
        gamepad: false,
    };

    /// A [`FocusScope`] which captures only keyboard input.
    pub const KEYBOARD: Self = Self {
        keyboard: true,
        gamepad: false,
    };

    /// A [`FocusScope`] which captures only gamepad input.
    pub const GAMEPAD: Self = Self {
        keyboard: false,
        gamepad: true,
    };

    /// Determines whether this scope overlaps with another scope.
    pub fn overlaps(self, other: Self) -> bool {
        self | other != Self::NONE
    }
}

impl std::ops::BitOr<FocusScope> for FocusScope {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self {
            keyboard: self.keyboard | rhs.keyboard,
            gamepad: self.gamepad | rhs.gamepad,
        }
    }
}

/// Describes an event that is localized to particular point in the user interface.
#[derive(Debug, Clone, Copy)]
pub enum CursorEvent {
    /// The state of a mouse button has changed.
    MouseButton { button: MouseButton, is_down: bool },

    /// The mouse wheel has been scrolled.
    MouseScroll(MouseScrollDelta),

    /// The cursor has moved by a given amount.
    Motion(Vector2),
}

/// Describes an event that is not associated with a spatial location in the user interface.
#[derive(Debug, Clone, Copy)]
pub enum GeneralEvent {
    /// The state of a key has changed.
    Key { key: Key, is_down: bool },
}

/// Identifies a button on a mouse.
pub type MouseButton = winit::event::MouseButton;

/// Describes an amount of scrolling done by a mouse or similar input device.
pub type MouseScrollDelta = winit::event::MouseScrollDelta;

/// Identifies/describes a keyboard key.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Key {
    pub scan_code: winit::event::ScanCode,
    pub key_code: Option<KeyCode>,
}

/// Symbolic identifier for a keyboard key.
pub use winit::event::VirtualKeyCode as KeyCode;

impl<'ui, Env: WidgetEnvironment + ?Sized, T: CursorInteractionHandler<'ui, Env> + ?Sized>
    CursorInteractionHandler<'ui, Env> for &T
{
    fn is_locked(&self, env: &Env) -> bool {
        (**self).is_locked(env)
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorInteractionEventResponse<'ui, Env> {
        (**self).cursor_event(env, pos, event)
    }

    fn general_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: GeneralEvent,
    ) -> CursorInteractionEventResponse<'ui, Env> {
        (**self).general_event(env, pos, event)
    }

    fn feedback(&self, env: &Env, f: &mut dyn FnMut(&dyn Any)) {
        (**self).feedback(env, f)
    }
}

impl<'ui, Env: WidgetEnvironment + ?Sized, T: FocusInteractionHandler<'ui, Env> + ?Sized>
    FocusInteractionHandler<'ui, Env> for &T
{
    fn general_event(
        &self,
        env: &mut Env,
        event: GeneralEvent,
    ) -> FocusInteractionEventResponse<'ui, Env> {
        (**self).general_event(env, event)
    }
}
