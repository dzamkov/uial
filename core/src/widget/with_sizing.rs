use crate::*;
use fortify::Lower;
use std::borrow::{Borrow, Cow};
use std::marker::PhantomData;

/// Contains with-sizing-related extension methods for [`Widget`].
pub trait WithSizingWidgetExt<S: State, G: Graphics>: Widget<S, G> + Sized {
    /// Constructs a [`Widget`] which imposes the size constraints and preferences from a
    /// state-dependent [`Sizing`] onto this widget.
    fn with_sizing<A: Dependent<S, Target = Sizing<i32>>>(
        &mut self,
        sizing: A,
    ) -> WithSizingWidget<S, G, Self, A> {
        WithSizingWidget(StateCache::from(IntersectSizing {
            _graphics_marker: PhantomData,
            source: self,
            sizing,
        }))
    }

    /// Constructs a [`Widget`] which imposes the size constraints and preferences from a
    /// [`Sizing`] onto this widget.
    fn with_sizing_const(
        &mut self,
        sizing: Sizing<i32>,
    ) -> WithSizingWidget<S, G, Self, Const<Sizing<i32>>> {
        self.with_sizing(Const::new(sizing))
    }

    /// Constructs a [`Widget`] which imposes a specific size onto this widget.
    fn with_size_const(
        &mut self,
        size: Vector2<i32>,
    ) -> WithSizingWidget<S, G, Self, Const<Sizing<i32>>> {
        self.with_sizing(Const::new(Sizing::exact(size)))
    }
}

impl<S: State, G: Graphics, T: Widget<S, G>> WithSizingWidgetExt<S, G> for T {}

/// A [`Widget`] which imposes the size constraints and preferences from a [`Sizing`] onto a source
/// widget.
pub struct WithSizingWidget<
    'a,
    S: State,
    G: Graphics,
    T: Widget<S, G>,
    A: Dependent<S, Target = Sizing<i32>>,
>(StateCache<S, IntersectSizing<'a, G, T, A>>);

struct IntersectSizing<'a, G, T, A> {
    _graphics_marker: PhantomData<fn(G)>,
    source: &'a T,
    sizing: A,
}

impl<'a, S: State, G: Graphics, T: Widget<S, G>, A: Dependent<S, Target = Sizing<i32>>> Dependent<S>
    for IntersectSizing<'a, G, T, A>
{
    type Target = Sizing<i32>;
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, Self::Target> {
        Cow::Owned(self.eval_own(s))
    }
}

impl<'a, S: State, G: Graphics, T: Widget<S, G>, A: Dependent<S, Target = Sizing<i32>>>
    OwnDependent<S> for IntersectSizing<'a, G, T, A>
{
    fn eval_own(&self, s: &S) -> Self::Target {
        let a = &*self.source.sizing(s);
        let b = &*self.sizing.eval(s);
        todo!()
    }
}

impl<'a, S: State, G: Graphics, T: Widget<S, G>, A: Dependent<S, Target = Sizing<i32>>> Widget<S, G>
    for WithSizingWidget<'a, S, G, T, A>
{
    type Elem<'b, P: Placement<State = S>>
    where
        Self: 'b,
    = T::Elem<'b, P>;

    fn sizing<'b>(&'b self, s: &'b S) -> Cow<'b, Sizing<i32>> {
        s.get_cache(&self.0)
    }

    fn place<'b, P: Placement<State = S>>(&'b self, s: &mut S, placement: P) -> Self::Elem<'b, P> {
        let sizing: &IntersectSizing<'a, G, T, A> = (*self.0).borrow();
        sizing.source.place(s, placement)
    }
}

unsafe impl<'a, 'b, S, G, T, A> Lower<'b> for WithSizingWidget<'a, S, G, T, A>
where
    S: State + 'b,
    G: Graphics + 'b,
    T: Widget<S, G> + Lower<'b>,
    <T as Lower<'b>>::Target: Widget<S, G> + Sized,
    A: Dependent<S, Target = Sizing<i32>> + Lower<'b>,
    <A as Lower<'b>>::Target: Dependent<S, Target = Sizing<i32>> + Sized,
{
    type Target = WithSizingWidget<'b, S, G, <T as Lower<'b>>::Target, <A as Lower<'b>>::Target>;
}
