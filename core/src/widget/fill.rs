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

impl WidgetBase for Fill {
    type Layout = Size2i;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        *layout
    }
}

impl<Env: WidgetEnvironment + ?Sized> Widget<Env> for Fill
where
    Env::Drawer: RasterDrawer,
{
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn layout(&self, _: &Env, size: Size2i) -> Self::Layout {
        size
    }

    fn relayout(&self, layout: &mut Self::Layout, _: &Env, size: Size2i) {
        *layout = size
    }

    fn draw(inst: WidgetInst<Self>, _: &Env, drawer: &mut Env::Drawer) {
        drawer.fill_rect(inst.widget.paint, inst.bounds())
    }
}
