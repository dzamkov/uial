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

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer)> WidgetBase
    for Canvas<Env, F>
{
    type Layout = Size2i;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        *layout
    }
}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&Env, Size2i, &mut Env::Drawer)> Widget<Env>
    for Canvas<Env, F>
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

    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer) {
        (inst.widget.draw)(env, *inst.layout, drawer)
    }
}