use crate::widget::*;
use crate::*;
use std::marker::PhantomData;

/// Constructs a [`Widget`] which runs a preparation function on initialization and defers to
/// another deferred-constructed widget.
pub fn prepare<F: FnOnce(&mut Interface<R::State>, &Interface<R::Graphics>) -> R, R: Widget>(
    init: F,
) -> PrepareWidget<F, R> {
    PrepareWidget {
        init,
        marker: PhantomData,
    }
}

/// A [`Widget`] which runs a preparation function on initialization and defers to another
/// deferred-constructed widget.
pub struct PrepareWidget<F, R: Widget> {
    init: F,
    marker: PhantomData<fn() -> R>,
}

impl<F: FnOnce(&mut Interface<R::State>, &Interface<R::Graphics>) -> R, R: Widget> Widget
    for PrepareWidget<F, R>
{
    type State = R::State;
    type Graphics = R::Graphics;
    type Inst<'a>
    where
        R::Graphics: 'a,
    = R::Inst<'a>;

    fn inst<'a>(
        self,
        s: &mut R::State,
        g: &'a R::Graphics,
    ) -> (R::Inst<'a>, <R::Inst<'a> as WidgetInst>::Key) {
        (self.init)(Interface::from_mut(s), Interface::from_ref(g)).inst(s, g)
    }
}
