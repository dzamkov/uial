use super::*;
use std::marker::PhantomData;

/// A non-interactive [`Widget`] which is drawn using a custom closure.
pub struct Canvas<Env: ?Sized, F> {
    _marker: PhantomData<fn(&Env)>,
    draw: F,
}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer)> Canvas<Env, F> {
    /// Constructs a new [`Canvas`] widget.
    pub fn new(draw: F) -> Self {
        Self {
            _marker: PhantomData,
            draw,
        }
    }
}

/// Constructs a non-interactive [`Widget`] which is drawn using the given closure.
pub fn canvas<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer)>(
    draw: F,
) -> Canvas<Env, F> {
    Canvas::new(draw)
}

impl<Env: ?Sized, F> WidgetBase for Canvas<Env, F> {}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer)> Widget<Env>
    for Canvas<Env, F>
{
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        CanvasInst { widget: self, slot }
    }
}

/// An instance of a [`Canvas`] widget.
struct CanvasInst<'a, Env: WidgetEnvironment + ?Sized, F, Slot> {
    widget: &'a Canvas<Env, F>,
    slot: Slot,
}

impl<
        'a,
        Env: WidgetEnvironment + ?Sized,
        F: Fn(&Env, Size2i, &mut Env::Drawer),
        Slot: WidgetSlot<Env>,
    > WidgetInst<Env> for CanvasInst<'a, Env, F, Slot>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        // TODO: Translate the drawer to the slot's minimum corner
        (self.widget.draw)(env, self.slot.size(env), drawer)
    }

    fn identify(&self, _: &Env, _: Vector2i) -> Option<WidgetId> {
        None
    }

    fn cursor_event(&self, _: &mut Env, _: Vector2i, _: CursorEvent) -> CursorEventResponse<Env> {
        CursorEventResponse::Bubble
    }

    fn focus(&self, _: &mut Env, _: bool) -> Option<FocusInteractionRequest<Env>> {
        None
    }
}
