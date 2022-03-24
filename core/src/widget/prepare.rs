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
pub struct PrepareWidget<F, R> {
    init: F,
    marker: PhantomData<fn() -> R>,
}

impl<F: FnOnce(&mut Interface<R::State>, &Interface<R::Graphics>) -> R, R: WidgetBase> WidgetBase
    for PrepareWidget<F, R>
{
    type State = R::State;
    type Graphics = R::Graphics;
}

impl<
        'a,
        F: FnOnce(&mut Interface<R::State>, &Interface<R::Graphics>) -> R,
        R: WidgetInst<'a>,
    > WidgetInst<'a> for PrepareWidget<F, R>
{
    type Inst = R::Inst;
    fn inst(
        self,
        s: &mut R::State,
        g: &'a R::Graphics,
    ) -> (R::Inst, <R::Inst as WidgetPlace>::Key) {
        (self.init)(Interface::from_mut(s), Interface::from_ref(g)).inst(s, g)
    }
}
