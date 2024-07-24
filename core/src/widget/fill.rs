use super::*;
use crate::drawer::RasterDrawer;

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

impl WidgetBase for Fill {}

impl<Env: WidgetEnvironment + ?Sized> Widget<Env> for Fill
where
    Env::Drawer: RasterDrawer,
{
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        FillInst {
            slot,
            paint: self.paint,
        }
    }
}

/// An instance of a [`Fill`] widget.
struct FillInst<Slot> {
    slot: Slot,
    paint: Paint,
}

impl<Env: WidgetEnvironment + ?Sized, Slot: WidgetSlot<Env>> WidgetInst<Env> for FillInst<Slot>
where
    Env::Drawer: RasterDrawer,
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        drawer.fill_rect(self.paint, self.slot.bounds(env))
    }

    fn cursor_event(&self, _: &mut Env, _: Vector2i, _: CursorEvent) -> CursorEventResponse<Env> {
        CursorEventResponse::Bubble
    }

    fn focus(&self, _: &mut Env, _: bool) -> Option<FocusInteractionRequest<Env>> {
        None
    }
}