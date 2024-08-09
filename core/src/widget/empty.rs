use super::*;

/// A completely transparent and uninteractive [`Widget`] that occupies a variable amount of
/// space.
pub struct Empty;

/// Get a [`Widget`] that is completely transparent and uninteractive, typically used to fill
/// space.
pub fn empty() -> Empty {
    Empty
}

impl<Env: WidgetEnvironment + ?Sized> Widget<Env> for Empty {
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, _: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        EmptyInst
    }
}

/// An instance of an [`Empty`] widget.
struct EmptyInst;

impl WidgetBase for Empty {}

impl<Env: WidgetEnvironment + ?Sized> WidgetInst<Env> for EmptyInst {
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
