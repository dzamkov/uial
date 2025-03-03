use crate::drawer::RasterDrawer;
use crate::prelude::*;
use std::any::Any;

/// A [`Widget`] that displays as a simple filled rectangle.
pub struct Fill {
    pub paint: Paint,
}

impl Fill {
    /// Constructs a new [`Fill`] widget.
    pub fn new(paint: Paint) -> Self {
        Self { paint }
    }
}

/// Constructs a [`Widget`] which displays as a simple filled rectangle.
pub fn fill(paint: Paint) -> Fill {
    Fill::new(paint)
}

impl WidgetLike for Fill {}

impl<Env: WidgetEnvironment + ?Sized> IntoWidget<Env> for Fill
where
    Env::Drawer: RasterDrawer,
{
    fn into_widget(self, _: &Env) -> impl Widget<Env> {
        self
    }
}

impl<Env: WidgetEnvironment + ?Sized> Widget<Env> for Fill
where
    Env::Drawer: RasterDrawer,
{
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        FillPlaced {
            slot,
            paint: self.paint,
        }
    }
}

/// A [`Fill`] widget which has been placed in a [`WidgetSlot`].
struct FillPlaced<Slot> {
    slot: Slot,
    paint: Paint,
}

impl<Env: WidgetEnvironment + ?Sized, Slot: WidgetSlot<Env>> WidgetPlaced<Env> for FillPlaced<Slot>
where
    Env::Drawer: RasterDrawer,
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        drawer.fill_rect(self.paint, self.slot.bounds(env))
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
