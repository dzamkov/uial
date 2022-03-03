use crate::*;
use fortify::Lower;
use std::borrow::Cow;

/// Contains with-sizing-related extension methods for [`Widget`].
pub trait WithSizingWidgetExt<S: State>: WidgetBase<S> + Sized {
    /// Imposes the size constraints and preferences from a state-dependent [`Sizing`] onto this
    /// widget.
    fn with_sizing_dep<A: Dependent<S, Target = Sizing>>(
        self,
        sizing: A,
    ) -> WithSizingWidget<S, Self, A> {
        WithSizingWidget(StateDerived::new(IntersectSizing {
            source: self,
            sizing,
        }))
    }

    /// Imposes the size constraints and preferences from a [`Sizing`] onto this widget.
    fn with_sizing(self, sizing: Sizing) -> WithSizingWidget<S, Self, Const<Sizing>> {
        self.with_sizing_dep(Const::new(sizing))
    }

    /// Imposes a specific size onto this widget.
    fn with_size(self, size: Vector2<u32>) -> WithSizingWidget<S, Self, Const<Sizing>> {
        self.with_sizing_dep(Const::new(Sizing::exact(size)))
    }
}

impl<S: State, T: WidgetBase<S>> WithSizingWidgetExt<S> for T {}

/// A [`Widget`] which imposes the size constraints and preferences from a [`Sizing`] onto a source
/// widget.
pub struct WithSizingWidget<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing>>(
    StateDerived<S, IntersectSizing<T, A>>,
);

struct IntersectSizing<T, A> {
    sizing: A,
    source: T,
}

impl<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing>> Dependent<S>
    for IntersectSizing<T, A>
{
    type Target = Sizing;
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, Self::Target> {
        Cow::Owned(self.eval_own(s))
    }
}

impl<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing>> OwnDependent<S>
    for IntersectSizing<T, A>
{
    fn eval_own(&self, s: &S) -> Self::Target {
        let a = &*self.source.sizing(s);
        let b = &*self.sizing.eval(s);
        Sizing::intersect(a, b)
    }
}

unsafe impl<'a, S, T, A> Lower<'a> for WithSizingWidget<S, T, A>
where
    S: State + 'a,
    T: WidgetBase<S> + Lower<'a>,
    <T as Lower<'a>>::Target: WidgetBase<S> + Sized,
    A: Dependent<S, Target = Sizing> + Lower<'a>,
    <A as Lower<'a>>::Target: Dependent<S, Target = Sizing> + Sized,
{
    type Target = WithSizingWidget<S, <T as Lower<'a>>::Target, <A as Lower<'a>>::Target>;
}

impl<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Sizing>> WidgetBase<S>
    for WithSizingWidget<S, T, A>
{
    fn sizing<'a>(&'a self, s: &'a S) -> Cow<'a, Sizing> {
        s.get_derived(&self.0)
    }
}

impl<S: State, G: Graphics, T: Widget<S, G>, A: Dependent<S, Target = Sizing>> Widget<S, G>
    for WithSizingWidget<S, T, A>
{
    type Elem<'a, P: Placement<State = S>>
    where
        Self: 'a,
    = T::Elem<'a, P>;

    fn place<'a, P: Placement<State = S>>(&'a self, s: &mut S, placement: P) -> Self::Elem<'a, P> {
        let sizing: &IntersectSizing<T, A> = self.0.source();
        sizing.source.place(s, placement)
    }
}
