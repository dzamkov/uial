use super::*;

/// A completely transparent and uninteractive [`Widget`] that occupies a variable amount of
/// space.
pub struct Empty;

/// Get a [`Widget`] that is completely transparent and uninteractive, typically used to fill
/// space.
pub fn empty() -> Empty {
    Empty
}

impl WidgetBase for Empty {
    type Layout = Size2i;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        *layout
    }
}

impl<Env: WidgetEnvironment + ?Sized> Widget<Env> for Empty
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

    fn draw(_: WidgetInst<Self>, _: &Env, _: &mut Env::Drawer) {
        // Nothing to do here
    }
}