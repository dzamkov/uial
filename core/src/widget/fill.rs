use crate::widget::*;
use crate::*;
use std::borrow::Cow;

/// Constructs a [`Widget`] that displays as a single solid [`Paint`] and is not interactable.
pub fn fill(paint: Paint) -> FillWidget {
    FillWidget(paint)
}

/// A [`Widget`] which displays as a single solid [`Paint`] and is not interactable.
#[derive(fortify::Lower)]
pub struct FillWidget(Paint);

impl WidgetBase for FillWidget {}

impl<S: State, G: Graphics> Widget<S, G> for FillWidget {
    type Inst<'a>
    where
        G: 'a,
    = FillWidget;
    
    fn inst(self, _: &mut S, _: &G) -> (FillWidget, ()) {
        (self, ())
    }
}

impl<S: State> WidgetInstBase<S> for FillWidget {
    fn sizing<'a>(&'a self, _: &'a S) -> Cow<'a, Sizing> {
        Cow::Owned(Sizing::any())
    }
}

impl<S: State, G: Graphics> WidgetInst<S, G> for FillWidget {
    type Key = ();

    type Elem<'a, P: Placement<State = S>>
    where
        Self: 'a,
    = FillElement<P>;

    fn place<'a, P: Placement<State = S>>(
        &'a self,
        _: &mut S,
        _: (),
        placement: P,
    ) -> Self::Elem<'a, P> {
        FillElement(self.0, placement)
    }
}

pub struct FillElement<P: Placement>(Paint, P);

impl<P: Placement, G: Graphics> Element<G> for FillElement<P> {
    type State = P::State;
    fn draw_to(&self, s: &Self::State, drawer: &mut G::Drawer<'_>) {
        drawer.fill_rect(self.0, self.1.rect(s))
    }
}
