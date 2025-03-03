use crate::prelude::*;
use std::any::Any;

/// A completely transparent and uninteractive [`Widget`] that occupies a variable amount of
/// space.
pub struct Empty;

/// Get a [`Widget`] that is completely transparent and uninteractive, typically used to fill
/// space.
pub fn empty() -> Empty {
    Empty
}

impl WidgetLike for Empty {}

impl<Env: WidgetEnvironment + ?Sized> IntoWidget<Env> for Empty {
    fn into_widget(self, _: &Env) -> impl Widget<Env> {
        self
    }
}

impl<Env: WidgetEnvironment + ?Sized> Widget<Env> for Empty {
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, _: S) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        EmptyPlaced
    }
}

/// An [`Empty`] widget which has been placed in a [`WidgetSlot`].
struct EmptyPlaced;

impl<Env: WidgetEnvironment + ?Sized> WidgetPlaced<Env> for EmptyPlaced {
    fn draw(&self, _: &Env, _: &mut Env::Drawer) {
        // Do nothing
    }

    fn hover_feedback(&self, _: &Env, _: Vector2i, _: &mut dyn FnMut(&dyn Any)) -> bool {
        false
    }

    fn cursor_event(&self, _: &mut Env, _: Vector2i, _: CursorEvent) -> CursorEventResponse<Env> {
        CursorEventResponse::Bubble
    }

    fn focus(&self, _: &mut Env, _: bool) -> Option<FocusInteractionRequest<Env>> {
        None
    }
}
