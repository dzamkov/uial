use crate::widget::*;
use crate::*;
use std::marker::PhantomData;

/// Constructs a [`Widget`] that displays a non-interactable [`State`]-dependent image.
pub fn image<S: State, G: Graphics, A: Dependent<S, Image<G>>>(image: A) -> ImageWidget<S, G, A> {
    ImageWidget {
        image,
        marker: PhantomData,
    }
}

/// A [`Widget`] which displays a not-interactable [`State`]-dependent image.
pub struct ImageWidget<S, G, A> {
    image: A,
    marker: PhantomData<fn(S, G)>,
}

impl<S: State, G: Graphics, A: Dependent<S, Image<G>>> WidgetBase for ImageWidget<S, G, A> {
    type State = S;
    type Graphics = G;
}

impl<'a, S: State, G: Graphics, A: Dependent<S, Image<G>>> WidgetInst<'a>
    for ImageWidget<S, G, A>
{
    type Inst = Self;
    fn inst(self, _: &mut S, _: &G) -> (Self, ()) {
        (self, ())
    }
}

impl<S: State, G: Graphics, A: Dependent<S, Image<G>>> WidgetPlace for ImageWidget<S, G, A> {
    type Key = ();
    type Elem<'a, P: Placement<State = Self::State>>
    where
        Self: 'a,
    = ImageElement<'a, A, P>;

    fn sizing<'a>(&'a self, s: &'a Self::State) -> Cow<'a, Sizing> {
        Cow::Owned(Sizing::exact(self.image.eval(s).size()))
    }

    fn place<'a, P: Placement<State = Self::State>>(
        &'a self,
        _: &mut Self::State,
        _: (),
        placement: P,
    ) -> Self::Elem<'a, P> {
        ImageElement {
            image: &self.image,
            placement,
        }
    }
}

unsafe impl<'a, S: 'a, G: 'a, A: Lower<'a>> Lower<'a> for ImageWidget<S, G, A>
where
    A::Target: Sized,
{
    type Target = ImageWidget<S, G, A::Target>;
}

pub struct ImageElement<'a, A, P: Placement> {
    image: &'a A,
    placement: P,
}

impl<'a, G: Graphics, P: Placement, A: Dependent<P::State, Image<G>>> Element<G>
    for ImageElement<'a, A, P>
{
    type State = P::State;
    fn draw_to(&self, s: &Self::State, drawer: &mut G::Drawer<'_>) {
        let rect = self.placement.rect(s);
        drawer.draw_image(
            &self.image.eval(s),
            Paint::white(),
            Box2::new(vec2(0, 0), rect.max - rect.min),
            GridAffine2::translation(rect.min),
        )
    }
}
