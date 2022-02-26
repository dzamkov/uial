use crate::*;
use std::borrow::Cow;

/// Constructs a [`Widget`] that displays as a single solid [`Paint`] and is not interactable.
pub fn fill(paint: Paint) -> FillWidget {
    FillWidget(paint)
}

#[derive(fortify::Lower)]
pub struct FillWidget(Paint);

impl<S: State> WidgetBase<S> for FillWidget {
    fn sizing(&self, _: &S) -> Cow<Sizing<i32>> {
        Cow::Owned(Sizing::any())
    }
}

impl<S: State, G: Graphics> Widget<S, G> for FillWidget {
    type Elem<'a, P: Placement<State = S>> = FillElement<P>;
    fn place<'a, P: Placement<State = S>>(&'a self, _: &mut S, placement: P) -> Self::Elem<'a, P> {
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
