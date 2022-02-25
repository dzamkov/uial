use crate::*;
use fortify::Lower;
use std::borrow::{Borrow, Cow};
use std::marker::PhantomData;

/// Contains with-sizing-related extension methods for [`Widget`].
pub trait WithSizingWidgetExt<S: State, D: Drawer>: Widget<S, D> + Sized {
    /// Constructs a [`Widget`] which imposes the size constraints and preferences from a
    /// state-dependent [`Sizing`] onto a source widget.
    fn with_sizing<A: Dependent<S, Target = Sizing<i32>>>(
        &self,
        sizing: A,
    ) -> WithSizingWidget<S, D, Self, A> {
        WithSizingWidget(StateCache::from(IntersectSizing {
            _drawer_marker: PhantomData,
            source: self,
            sizing,
        }))
    }

    /// Constructs a [`Widget`] which imposes the size constraints and preferences from a
    /// [`Sizing`] onto a source widget.
    fn with_sizing_const(
        &self,
        sizing: Sizing<i32>,
    ) -> WithSizingWidget<S, D, Self, Const<Sizing<i32>>> {
        self.with_sizing(Const::new(sizing))
    }

    fn with_size_const(
        &self,
        size: Vector2<i32>,
    ) -> WithSizingWidget<S, D, Self, Const<Sizing<i32>>> {
        self.with_sizing(Const::new(Sizing::exact(size)))
    }
}

impl<S: State, D: Drawer, T: Widget<S, D>> WithSizingWidgetExt<S, D> for T {}

/// A [`Widget`] which imposes the size constraints and preferences from a [`Sizing`] onto a source
/// widget.
pub struct WithSizingWidget<
    'a,
    S: State,
    D: Drawer,
    T: Widget<S, D>,
    A: Dependent<S, Target = Sizing<i32>>,
>(StateCache<S, IntersectSizing<'a, D, T, A>>);

struct IntersectSizing<'a, D, T, A> {
    _drawer_marker: PhantomData<fn(D)>,
    source: &'a T,
    sizing: A,
}

impl<'a, S: State, D: Drawer, T: Widget<S, D>, A: Dependent<S, Target = Sizing<i32>>> Dependent<S>
    for IntersectSizing<'a, D, T, A>
{
    type Target = Sizing<i32>;
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, Self::Target> {
        Cow::Owned(self.eval_own(s))
    }
}

impl<'a, S: State, D: Drawer, T: Widget<S, D>, A: Dependent<S, Target = Sizing<i32>>>
    OwnDependent<S> for IntersectSizing<'a, D, T, A>
{
    fn eval_own(&self, s: &S) -> Self::Target {
        let a = &*self.source.sizing(s);
        let b = &*self.sizing.eval(s);
        todo!()
    }
}

impl<'a, S: State, D: Drawer, T: Widget<S, D>, A: Dependent<S, Target = Sizing<i32>>> Widget<S, D>
    for WithSizingWidget<'a, S, D, T, A>
{
    type Elem<'b, P: Placement<State = S>>
    where
        Self: 'b,
    = T::Elem<'b, P>;

    fn sizing<'b>(&'b self, s: &'b S) -> Cow<'b, Sizing<i32>> {
        s.get_cache(&self.0)
    }

    fn place<'b, P: Placement<State = S>>(&'b self, s: &mut S, placement: P) -> Self::Elem<'b, P> {
        let sizing: &IntersectSizing<'a, D, T, A> = (*self.0).borrow();
        sizing.source.place(s, placement)
    }
}

unsafe impl<'a, 'b, S, D, T, A> Lower<'b> for WithSizingWidget<'a, S, D, T, A>
where
    S: State + 'b,
    D: Drawer + 'b,
    T: Widget<S, D> + Lower<'b>,
    <T as Lower<'b>>::Target: Widget<S, D> + Sized,
    A: Dependent<S, Target = Sizing<i32>> + Lower<'b>,
    <A as Lower<'b>>::Target: Dependent<S, Target = Sizing<i32>> + Sized,
{
    type Target = WithSizingWidget<'b, S, D, <T as Lower<'b>>::Target, <A as Lower<'b>>::Target>;
}
