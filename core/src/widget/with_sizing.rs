use crate::widget::*;
use crate::*;
use std::borrow::Cow;

/// Contains with-sizing-related extension methods for [`Widget`].
pub trait WithSizingWidgetExt: WidgetBase + Sized {
    /// Imposes the size constraints and preferences from a state-dependent [`Sizing`] onto this
    /// widget.
    fn with_sizing<A>(self, sizing: A) -> WithSizingWidget<Self, A> {
        WithSizingWidget {
            source: self,
            sizing,
        }
    }

    /// Imposes a specific size onto this widget.
    fn with_size(self, size: Vector2<u32>) -> WithSizingWidget<Self, Sizing> {
        self.with_sizing(Sizing::exact(size))
    }
}

impl<T: WidgetBase> WithSizingWidgetExt for T {}

/// A [`Widget`] which imposes the size constraints and preferences from a [`Sizing`] onto a source
/// widget.
pub struct WithSizingWidget<T, A> {
    source: T,
    sizing: A,
}

impl<T: WidgetBase, A> WidgetBase for WithSizingWidget<T, A> {}

impl<S: State, G: Graphics, T: Widget<S, G>, A: Dependent<S, Sizing>> Widget<S, G>
    for WithSizingWidget<T, A>
{
    type Inst<'a>
    where
        G: 'a,
    = WithSizingWidgetInst<S, T::Inst<'a>, A>;
    
    fn inst<'a>(
        self,
        s: &mut S,
        g: &'a G,
    ) -> (Self::Inst<'a>, <Self::Inst<'a> as WidgetInst<S, G>>::Key) {
        let (source, key) = self.source.inst(s, g);
        let sizing = self.sizing;
        let inst = WithSizingWidgetInst(StateDerived::new(IntersectSizing { source, sizing }));
        (inst, key)
    }
}

/// An instance of a [`WithSizingWidget`].
pub struct WithSizingWidgetInst<S: State, T: WidgetInstBase<S>, A: Dependent<S, Sizing>>(
    StateDerived<S, IntersectSizing<T, A>>,
);

struct IntersectSizing<T, A> {
    source: T,
    sizing: A,
}

impl<S: State, T: WidgetInstBase<S>, A: Dependent<S, Sizing>> DerivedFn<S>
    for IntersectSizing<T, A>
{
    type Target = Sizing;
    fn eval(&self, s: &S) -> Self::Target {
        let a = &*self.source.sizing(s);
        let b = &*self.sizing.eval(s);
        Sizing::intersect(a, b)
    }
}

impl<S: State, T: WidgetInstBase<S>, A: Dependent<S, Sizing>> WidgetInstBase<S>
    for WithSizingWidgetInst<S, T, A>
{
    fn sizing<'a>(&'a self, s: &'a S) -> Cow<'a, Sizing> {
        s.get_derived(&self.0)
    }
}

impl<S: State, G: Graphics, T: WidgetInst<S, G>, A: Dependent<S, Sizing>> WidgetInst<S, G>
    for WithSizingWidgetInst<S, T, A>
{
    type Key = T::Key;

    type Elem<'a, P: Placement<State = S>>
    where
        Self: 'a,
    = T::Elem<'a, P>;

    fn place<'a, P: Placement<State = S>>(
        &'a self,
        s: &mut S,
        key: Self::Key,
        placement: P,
    ) -> Self::Elem<'a, P> {
        let sizing: &IntersectSizing<T, A> = self.0.source();
        sizing.source.place(s, key, placement)
    }
}
