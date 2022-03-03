use crate::*;
use fortify::Lower;
use std::borrow::Cow;

/// Contains with-padding-related extension methods for [`Widget`].
pub trait WithPaddingWidgetExt<S: State>: WidgetBase<S> + Sized {
    /// Applies a state-dependent amount of [`Padding`] onto this widget.
    fn with_padding_dep<A: Dependent<S, Target = Padding>>(
        self,
        padding: A,
    ) -> WithPaddingWidget<S, Self, A> {
        WithPaddingWidget(StateDerived::new(PaddingSizing {
            source: self,
            padding,
        }))
    }

    /// Applies an amount of [`Padding`] onto this widget.
    fn with_padding(self, padding: Padding) -> WithPaddingWidget<S, Self, Const<Padding>> {
        self.with_padding_dep(Const::new(padding))
    }

    /// Applies variable-sized padding to place this widget within a larger rectangle. The
    /// alignment is specified by the relative portion of padding (between 0 and 1) on the left and
    /// top of the widget. For example, a value of 0 for `left` will place the inner widget as far
    /// left as possible, while a value of 0.5 will center it along the horizontal axis.
    fn with_align(self, left: f32, top: f32) -> WithPaddingWidget<S, Self, Const<Padding>> {
        self.with_padding(Padding {
            left: 0,
            right: 0,
            top: 0,
            bottom: 0,
            stretch_left: Some(left),
            stretch_top: Some(top),
        })
    }
}

impl<S: State, T: WidgetBase<S>> WithPaddingWidgetExt<S> for T {}

/// Specifies an amount of padding that can be applied to rectangular content.
#[derive(Lower, Clone, Copy)]
pub struct Padding {
    /// The amount of fixed padding applied to the left of the inner content.
    pub left: u32,

    /// The amount of fixed padding applied to the right of the inner content.
    pub right: u32,

    /// The amount of fixed padding applied to the top of the inner content.
    pub top: u32,

    /// The amount of fixed padding applied to the bottom of the inner content.
    pub bottom: u32,

    // If [`Some`], indicates that a variable amount of padding is applied on the left and right
    // of the inner content, and specifies the proportion (between 0 and 1) of this extra padding
    // that is applied on the left.
    pub stretch_left: Option<f32>,

    // If [`Some`], indicates that a variable amount of padding is applied on the top and bottom
    // of the inner content, and specifies the proportion (between 0 and 1) of this extra padding
    // that is applied on the top.
    pub stretch_top: Option<f32>,
}

/// A [`Widget`] which applies a state-dependent amount of [`Padding`] onto a source widget.
pub struct WithPaddingWidget<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Padding>>(
    StateDerived<S, PaddingSizing<T, A>>,
);

struct PaddingSizing<T, A> {
    source: T,
    padding: A,
}

impl<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Padding>> Dependent<S>
    for PaddingSizing<T, A>
{
    type Target = Sizing;
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, Self::Target> {
        Cow::Owned(self.eval_own(s))
    }
}

impl<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Padding>> OwnDependent<S>
    for PaddingSizing<T, A>
{
    fn eval_own(&self, s: &S) -> Self::Target {
        let sizing = &*self.source.sizing(s);
        let padding = &*self.padding.eval(s);
        sizing.with_padding(
            padding.left + padding.right,
            padding.top + padding.bottom,
            padding.stretch_left.is_some(),
            padding.stretch_top.is_some(),
        )
    }
}

unsafe impl<'a, S, T, A> Lower<'a> for WithPaddingWidget<S, T, A>
where
    S: State + 'a,
    T: WidgetBase<S> + Lower<'a>,
    <T as Lower<'a>>::Target: WidgetBase<S> + Sized,
    A: Dependent<S, Target = Padding> + Lower<'a>,
    <A as Lower<'a>>::Target: Dependent<S, Target = Padding> + Sized,
{
    type Target = WithPaddingWidget<S, <T as Lower<'a>>::Target, <A as Lower<'a>>::Target>;
}

impl<S: State, T: WidgetBase<S>, A: Dependent<S, Target = Padding>> WidgetBase<S>
    for WithPaddingWidget<S, T, A>
{
    fn sizing<'a>(&'a self, s: &'a S) -> Cow<'a, Sizing> {
        s.get_derived(&self.0)
    }
}

impl<S: State, G: Graphics, T: Widget<S, G>, A: Dependent<S, Target = Padding>> Widget<S, G>
    for WithPaddingWidget<S, T, A>
{
    type Elem<'a, P: Placement<State = S>>
    where
        Self: 'a,
    = T::Elem<'a, WithPaddingPlacement<'a, S, T, A, P>>;

    fn place<'a, P: Placement<State = S>>(&'a self, s: &mut S, placement: P) -> Self::Elem<'a, P> {
        let sizing: &PaddingSizing<T, A> = self.0.source();
        sizing.source.place(
            s,
            WithPaddingPlacement(StateDerived::new(PaddingRect {
                padding: &sizing.padding,
                sizing: &self.0,
                placement,
            })),
        )
    }
}

/// The placement for an inner [`Widget`] inside a [`WithPaddingWidget`].
pub struct WithPaddingPlacement<
    'a,
    S: State,
    T: WidgetBase<S>,
    A: Dependent<S, Target = Padding>,
    P: Placement<State = S>,
>(StateDerived<S, PaddingRect<'a, S, T, A, P>>);

impl<
        'a,
        S: State,
        T: WidgetBase<S>,
        A: Dependent<S, Target = Padding>,
        P: Placement<State = S>,
    > Placement for WithPaddingPlacement<'a, S, T, A, P>
{
    type State = S;
    fn rect(&self, s: &Self::State) -> Box2<i32> {
        *s.get_derived(&self.0)
    }
}

struct PaddingRect<
    'a,
    S: State,
    T: WidgetBase<S>,
    A: Dependent<S, Target = Padding>,
    P: Placement<State = S>,
> {
    padding: &'a A,
    sizing: &'a StateDerived<S, PaddingSizing<T, A>>,
    placement: P,
}

impl<
        'a,
        S: State,
        T: WidgetBase<S>,
        A: Dependent<S, Target = Padding>,
        P: Placement<State = S>,
    > Dependent<S> for PaddingRect<'a, S, T, A, P>
{
    type Target = Box2<i32>;
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, Self::Target> {
        Cow::Owned(self.eval_own(s))
    }
}

impl<
        'a,
        S: State,
        T: WidgetBase<S>,
        A: Dependent<S, Target = Padding>,
        P: Placement<State = S>,
    > OwnDependent<S> for PaddingRect<'a, S, T, A, P>
{
    fn eval_own(&self, s: &S) -> Self::Target {
        let padding = &*self.padding.eval(s);
        let sizing = &*self.sizing.eval(s);
        let container = self.placement.rect(s);
        let padded_size = container.size();
        let mut min = container.min + vec2(padding.left as i32, padding.bottom as i32);
        let padded_size = vec2(padded_size.x as u32, padded_size.y as u32);
        let width = match padding.stretch_left {
            Some(f) => {
                let stretch_width = sizing.stretch_width(padded_size);
                min.x += (stretch_width as f32 * f) as i32;
                padded_size.x - stretch_width
            }
            None => padded_size.x,
        } - (padding.left + padding.right);
        let height = match padding.stretch_top {
            Some(f) => {
                let stretch_height = sizing.stretch_height(padded_size);
                min.y += (stretch_height as f32 * (1.0 - f)) as i32;
                padded_size.y - stretch_height
            }
            None => padded_size.y,
        } - (padding.top + padding.bottom);
        Box2::new(min, min + vec2(width as i32, height as i32))
    }
}
