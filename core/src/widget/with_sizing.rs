use crate::widget::*;
use crate::*;
use std::borrow::Cow;

/// Contains with-sizing-related extension methods for [`Widget`].
pub trait WithSizingWidgetExt: Widget + Sized {
    /// Imposes the size constraints and preferences from a state-dependent [`Sizing`] onto this
    /// widget.
    fn with_sizing<A: Dependent<Self::State, Sizing>>(
        self,
        sizing: A,
    ) -> WithSizingWidget<Self, A> {
        WithSizingWidget {
            source: self,
            sizing,
        }
    }

    /// Imposes a specific size onto this widget.
    fn with_size<A: Dependent<Self::State, Vector2<u32>>>(
        self,
        size: A,
    ) -> WithSizingWidget<Self, ExactSizing<A>> {
        self.with_sizing(ExactSizing(size))
    }
}

impl<T: Widget> WithSizingWidgetExt for T {}

/// A [`Widget`] which imposes the size constraints and preferences from a [`Sizing`] onto a source
/// widget.
pub struct WithSizingWidget<T, A> {
    source: T,
    sizing: A,
}

impl<T: Widget, A: Dependent<T::State, Sizing>> Widget for WithSizingWidget<T, A> {
    type State = T::State;
    type Graphics = T::Graphics;
    type Inst<'a>
    where
        T::Graphics: 'a,
    = WithSizingWidgetInst<T::Inst<'a>, A>;

    fn inst<'a>(
        self,
        s: &mut T::State,
        g: &'a T::Graphics,
    ) -> (Self::Inst<'a>, <Self::Inst<'a> as WidgetInst>::Key) {
        let (source, key) = self.source.inst(s, g);
        let sizing = self.sizing;
        let inst = WithSizingWidgetInst(StateDerived::new(IntersectSizing { source, sizing }));
        (inst, key)
    }
}

/// An instance of a [`WithSizingWidget`].
pub struct WithSizingWidgetInst<T: WidgetInst, A: Dependent<T::State, Sizing>>(
    StateDerived<T::State, IntersectSizing<T, A>>,
);

struct IntersectSizing<T, A> {
    source: T,
    sizing: A,
}

impl<T: WidgetInst, A: Dependent<T::State, Sizing>> DerivedFn<T::State> for IntersectSizing<T, A> {
    type Target = Sizing;
    fn eval(&self, s: &T::State) -> Self::Target {
        let a = &*self.source.sizing(s);
        let b = &*self.sizing.eval(s);
        Sizing::intersect(a, b)
    }
}

impl<T: WidgetInst, A: Dependent<T::State, Sizing>> WidgetInst for WithSizingWidgetInst<T, A> {
    type State = T::State;
    type Graphics = T::Graphics;
    type Key = T::Key;
    type Elem<'a, P: Placement<State = T::State>>
    where
        Self: 'a,
    = T::Elem<'a, P>;

    fn sizing<'a>(&'a self, s: &'a T::State) -> Cow<'a, Sizing> {
        s.get_derived(&self.0)
    }

    fn place<'a, P: Placement<State = T::State>>(
        &'a self,
        s: &mut T::State,
        key: Self::Key,
        placement: P,
    ) -> Self::Elem<'a, P> {
        let sizing: &IntersectSizing<T, A> = self.0.source();
        sizing.source.place(s, key, placement)
    }
}

/// A state-dependent [`Sizing`] produced by calling [`Sizing::exact`] on a state-dependent
/// size.
pub struct ExactSizing<A>(A);

impl<S: State, A: Dependent<S, Vector2<u32>>> Dependent<S, Sizing> for ExactSizing<A> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, Sizing> {
        Cow::Owned(Sizing::exact(*self.0.eval(s)))
    }
}