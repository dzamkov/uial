use crate::prelude::*;
use std::any::Any;
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

impl<Env: ?Sized, F> WidgetLike for Canvas<Env, F> {}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer)> IntoWidget<Env>
    for Canvas<Env, F>
{
    fn into_widget(self, _: &Env) -> impl Widget<Env> {
        self
    }
}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer)> Widget<Env>
    for Canvas<Env, F>
{
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        CanvasPlaced { widget: self, slot }
    }
}

/// A [`Canvas`] widget which has been placed in a [`WidgetSlot`].
struct CanvasPlaced<'a, Env: WidgetEnvironment + ?Sized, F, Slot> {
    widget: &'a Canvas<Env, F>,
    slot: Slot,
}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer), Slot: WidgetSlot<Env>>
    WidgetPlaced<Env> for CanvasPlaced<'_, Env, F, Slot>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        // TODO: Translate the drawer to the slot's minimum corner
        (self.widget.draw)(env, self.slot.size(env), drawer)
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
