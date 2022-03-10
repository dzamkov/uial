use crate::widget::*;
use crate::*;
use std::borrow::Cow;
use std::marker::PhantomData;

/// Constructs a [`Widget`] that displays as a single solid [`Paint`] and is not interactable.
pub fn fill<S: State, G: Graphics>(paint: Paint) -> FillWidget<S, G> {
    FillWidget {
        paint,
        marker: PhantomData,
    }
}

/// A [`Widget`] which displays as a single solid [`Paint`] and is not interactable.
#[derive(fortify::Lower)]
pub struct FillWidget<S: State, G: Graphics> {
    paint: Paint,
    marker: PhantomData<fn(S, G)>,
}

impl<S: State, G: Graphics> Widget for FillWidget<S, G> {
    type State = S;
    type Graphics = G;
    type Inst<'a>
    where
        G: 'a,
    = FillWidget<S, G>;

    fn inst(self, _: &mut S, _: &G) -> (FillWidget<S, G>, ()) {
        (self, ())
    }
}

impl<S: State, G: Graphics> WidgetInst for FillWidget<S, G> {
    type State = S;
    type Graphics = G;
    type Key = ();
    type Elem<'a, P: Placement<State = S>>
    where
        Self: 'a,
    = FillElement<P>;

    fn sizing<'a>(&'a self, _: &'a S) -> Cow<'a, Sizing> {
        Cow::Owned(Sizing::any())
    }

    fn place<'a, P: Placement<State = S>>(
        &'a self,
        _: &mut S,
        _: (),
        placement: P,
    ) -> Self::Elem<'a, P> {
        FillElement(self.paint, placement)
    }
}

pub struct FillElement<P: Placement>(Paint, P);

impl<P: Placement, G: Graphics> Element<G> for FillElement<P> {
    type State = P::State;
    fn draw_to(&self, s: &Self::State, drawer: &mut G::Drawer<'_>) {
        drawer.fill_rect(self.0, self.1.rect(s))
    }
}
