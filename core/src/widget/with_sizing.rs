use crate::*;
use fortify::Lower;
use std::borrow::{Borrow, Cow};

/// Contains with-sizing-related extension methods for [`Widget`].
pub trait WithSizingWidgetExt<S: State>: WidgetBase<S> + Sized {
    /// Constructs a [`Widget`] which imposes the size constraints and preferences from a
    /// state-dependent [`Sizing`] onto this widget.
    fn with_sizing<A: Dependent<S, Target = Sizing<i32>>>(
        &mut self,
        sizing: A,
    ) -> WithSizingWidget<S, Self, A> {
        WithSizingWidget(StateCache::from(IntersectSizing {
            source: self,
            sizing,
        }))
    }

    /// Constructs a [`Widget`] which imposes the size constraints and preferences from a
    /// [`Sizing`] onto this widget.
    fn with_sizing_const(
        &mut self,
        sizing: Sizing<i32>,
    ) -> WithSizingWidget<S, Self, Const<Sizing<i32>>> {
        self.with_sizing(Const::new(sizing))
    }

    /// Constructs a [`Widget`] which imposes a specific size onto this widget.
    fn with_size_const(
        &mut self,
        size: Vector2<i32>,
    ) -> WithSizingWidget<S, Self, Const<Sizing<i32>>> {
        self.with_sizing(Const::new(Sizing::exact(size)))
    }
}

impl<S: State, T: WidgetBase<S>> WithSizingWidgetExt<S> for T {}

/// A [`Widget`] which imposes the size constraints and preferences from a [`Sizing`] onto a source
/// widget.
pub struct WithSizingWidget<'a, S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing<i32>>>(
    StateCache<S, IntersectSizing<'a, T, A>>,
);

struct IntersectSizing<'a, T, A> {
    source: &'a T,
    sizing: A,
}

impl<'a, S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing<i32>>> Dependent<S>
    for IntersectSizing<'a, T, A>
{
    type Target = Sizing<i32>;
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, Self::Target> {
        Cow::Owned(self.eval_own(s))
    }
}

impl<'a, S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing<i32>>> OwnDependent<S>
    for IntersectSizing<'a, T, A>
{
    fn eval_own(&self, s: &S) -> Self::Target {
        let a = &*self.source.sizing(s);
        let b = &*self.sizing.eval(s);
        todo!()
    }
}

impl<'a, S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing<i32>>> WidgetBase<S>
    for WithSizingWidget<'a, S, T, A>
{
    fn sizing<'b>(&'b self, s: &'b S) -> Cow<'b, Sizing<i32>> {
        s.get_cache(&self.0)
    }
}

impl<'a, S: State, G: Graphics, T: Widget<S, G>, A: Dependent<S, Target = Sizing<i32>>> Widget<S, G>
    for WithSizingWidget<'a, S, T, A>
{
    type Elem<'b, P: Placement<State = S>>
    where
        Self: 'b,
    = T::Elem<'b, P>;

    fn place<'b, P: Placement<State = S>>(&'b self, s: &mut S, placement: P) -> Self::Elem<'b, P> {
        let sizing: &IntersectSizing<'a, T, A> = (*self.0).borrow();
        sizing.source.place(s, placement)
    }
}

unsafe impl<'a, 'b, S, T, A> Lower<'b> for WithSizingWidget<'a, S, T, A>
where
    S: State + 'b,
    T: WidgetBase<S> + Lower<'b>,
    <T as Lower<'b>>::Target: WidgetBase<S> + Sized,
    A: Dependent<S, Target = Sizing<i32>> + Lower<'b>,
    <A as Lower<'b>>::Target: Dependent<S, Target = Sizing<i32>> + Sized,
{
    type Target = WithSizingWidget<'b, S, <T as Lower<'b>>::Target, <A as Lower<'b>>::Target>;
}
