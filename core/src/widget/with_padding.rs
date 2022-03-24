use crate::widget::*;
use crate::*;
use fortify::Lower;
use std::borrow::Cow;

/// Contains with-padding-related extension methods for [`Widget`].
pub trait WithPaddingWidgetExt: Widget + Sized {
    /// Applies a state-dependent amount of [`Padding`] onto this widget.
    fn with_padding<A: Dependent<Self::State, Padding>>(
        self,
        padding: A,
    ) -> WithPaddingWidget<Self, A> {
        WithPaddingWidget {
            source: self,
            padding,
        }
    }

    /// Applies variable-sized padding to place this widget within a larger rectangle. The
    /// alignment is specified by the relative portion of padding (between 0 and 1) on the left and
    /// top of the widget. For example, a value of 0 for `left` will place the inner widget as far
    /// left as possible, while a value of 0.5 will center it along the horizontal axis.
    fn with_align(self, left: f32, top: f32) -> WithPaddingWidget<Self, Padding> {
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

impl<T: Widget> WithPaddingWidgetExt for T {}

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
pub struct WithPaddingWidget<T, A> {
    source: T,
    padding: A,
}

impl<T: WidgetBase, A: Dependent<T::State, Padding>> WidgetBase for WithPaddingWidget<T, A> {
    type State = T::State;
    type Graphics = T::Graphics;
}

impl<'a, T: WidgetInst<'a>, A: Dependent<T::State, Padding>> WidgetInst<'a>
    for WithPaddingWidget<T, A>
{
    type Inst = WithPaddingWidgetInst<T::Inst, A>;

    fn inst(
        self,
        s: &mut T::State,
        g: &'a T::Graphics,
    ) -> (Self::Inst, <Self::Inst as WidgetPlace>::Key) {
        let (source, key) = self.source.inst(s, g);
        let padding = self.padding;
        let inst = WithPaddingWidgetInst(StateDerived::new(PaddingSizing { source, padding }));
        (inst, key)
    }
}

unsafe impl<'a, T: Lower<'a>, A: Lower<'a>> Lower<'a> for WithPaddingWidget<T, A>
where
    T::Target: Sized,
    A::Target: Sized,
{
    type Target = WithPaddingWidget<T::Target, A::Target>;
}

/// An instance of a [`WithPaddingWidget`].
pub struct WithPaddingWidgetInst<T: WidgetPlace, A: Dependent<T::State, Padding>>(
    StateDerived<T::State, PaddingSizing<T, A>>,
);

struct PaddingSizing<T, A> {
    source: T,
    padding: A,
}

impl<T: WidgetPlace, A: Dependent<T::State, Padding>> DerivedFn<T::State> for PaddingSizing<T, A> {
    type Target = Sizing;
    fn eval<'a>(&'a self, s: &'a T::State) -> Self::Target {
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

impl<T: WidgetPlace, A: Dependent<T::State, Padding>> WidgetBase for WithPaddingWidgetInst<T, A> {
    type State = T::State;
    type Graphics = T::Graphics;
}

impl<T: WidgetPlace, A: Dependent<T::State, Padding>> WidgetPlace for WithPaddingWidgetInst<T, A> {
    type Key = T::Key;
    type Elem<'a, P: Placement<State = T::State>>
    where
        Self: 'a,
    = T::Elem<'a, WithPaddingPlacement<'a, T, A, P>>;

    fn sizing<'a>(&'a self, s: &'a T::State) -> Cow<'a, Sizing> {
        s.get_derived(&self.0)
    }

    fn place<'a, P: Placement<State = T::State>>(
        &'a self,
        s: &mut T::State,
        key: Self::Key,
        placement: P,
    ) -> Self::Elem<'a, P> {
        let sizing: &PaddingSizing<T, A> = self.0.source();
        sizing.source.place(
            s,
            key,
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
    T: WidgetPlace,
    A: Dependent<T::State, Padding>,
    P: Placement<State = T::State>,
>(StateDerived<T::State, PaddingRect<'a, T, A, P>>);

impl<'a, T: WidgetPlace, A: Dependent<T::State, Padding>, P: Placement<State = T::State>> Placement
    for WithPaddingPlacement<'a, T, A, P>
{
    type State = T::State;
    fn rect(&self, s: &Self::State) -> Box2<i32> {
        *s.get_derived(&self.0)
    }
}

struct PaddingRect<
    'a,
    T: WidgetPlace,
    A: Dependent<T::State, Padding>,
    P: Placement<State = T::State>,
> {
    padding: &'a A,
    sizing: &'a StateDerived<T::State, PaddingSizing<T, A>>,
    placement: P,
}

impl<'a, T: WidgetPlace, A: Dependent<T::State, Padding>, P: Placement<State = T::State>>
    DerivedFn<T::State> for PaddingRect<'a, T, A, P>
{
    type Target = Box2<i32>;
    fn eval<'b>(&'b self, s: &'b T::State) -> Self::Target {
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
