use crate::widget::*;
use crate::*;

/// Constructs a [`Widget`] which runs a preparation function on initialization and defers to
/// another deferred-constructed widget.
pub fn prepare<
    S: State,
    G: Graphics,
    F: FnOnce(&mut Interface<S>, &Interface<G>) -> R,
    R: Widget<S, G>,
>(
    init: F,
) -> PrepareWidget<F> {
    PrepareWidget(init)
}

/// A [`Widget`] which runs a preparation function on initialization and defers to another
/// deferred-constructed widget.
pub struct PrepareWidget<F>(F);

impl<F> WidgetBase for PrepareWidget<F> {}

impl<S: State, G: Graphics, F: FnOnce(&mut Interface<S>, &Interface<G>) -> R, R: Widget<S, G>>
    Widget<S, G> for PrepareWidget<F>
{
    type Inst = R::Inst;
    fn inst(self, s: &mut S, g: &G) -> (Self::Inst, <R::Inst as WidgetInst<S, G>>::Key) {
        (self.0)(Interface::from_mut(s), Interface::from_ref(g)).inst(s, g)
    }
}
