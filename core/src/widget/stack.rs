use crate::prelude::*;
use crate::RationalU32;
use std::any::Any;
use std::rc::Rc;

// TODO: Allow weights to be specified in macro
// TODO: Create balanced `Stack` tree from list of parts
/// Constructs a [`Widget`] by "stacking" the given [`Widget`]s horizontally such that they all
/// have the same height.
#[macro_export]
macro_rules! stack_h {
    [$head:expr, $($tail:expr),*] => {
        {
            let res = $crate::widget::Weighted::new($head, 1);
            $(
                let res = $crate::widget::Stack::<_, _, false>::new_weighted(
                    res,
                    $crate::widget::Weighted::new($tail, 1)
                );
            )*
            res.source
        }
    }
}

/// Constructs a [`Widget`] by "stacking" the given [`Widget`]s vertically such that they all
/// have the same width.
#[macro_export]
macro_rules! stack_v {
    [$head:expr, $($tail:expr),*] => {
        {
            let res = $crate::widget::Weighted::new($head, 1);
            $(
                let res = $crate::widget::Stack::<_, _, true>::new_weighted(
                    $crate::widget::Weighted::new($tail, 1),
                    res
                );
            )*
            res.source
        }
    }
}

/// Wraps a [`Widget`], pairing it with a "weight" value. The weight is used to determine the
/// relative size of a [`Widget`] when it is stacked with other widgets.
pub struct Weighted<T> {
    pub source: T,
    pub weight: u32,
}

impl<T> Weighted<T> {
    /// Constructs a new weighted widget.
    pub fn new(source: T, weight: u32) -> Self {
        Weighted { source, weight }
    }
}

/// A [`Widget`] which combines two source [`Widget`]s by stacking them either horizontally or
/// vertically.
pub struct Stack<A, B, const VERTICAL: bool> {
    a: A,
    b: B,
    split: RationalU32,
}

impl<A, B, const VERTICAL: bool> Stack<A, B, VERTICAL> {
    /// Constructs a [`Stack`] widget.
    pub fn new(a: A, b: B, split: RationalU32) -> Self {
        assert!(
            split <= 1.into(),
            "`split` must be at most 1. `split` = {}",
            split
        );
        Self { a, b, split }
    }

    /// Constructs a [`Stack`] widget using the weights of the parts to allocate space between
    /// them.
    pub fn new_weighted(a: Weighted<A>, b: Weighted<B>) -> Weighted<Self> {
        let weight = a.weight + b.weight;
        Weighted {
            source: Self {
                a: a.source,
                b: b.source,
                split: RationalU32::new(a.weight, weight),
            },
            weight,
        }
    }
}

impl<A: WidgetBase, B: WidgetBase, const VERTICAL: bool> WidgetBase for Stack<A, B, VERTICAL> {}

impl<
        Env: WidgetEnvironment + Track + ?Sized,
        A: Widget<Env>,
        B: Widget<Env>,
        const VERTICAL: bool,
    > Widget<Env> for Stack<A, B, VERTICAL>
{
    fn sizing(&self, env: &Env) -> Sizing {
        let sizing_a = self.a.sizing(env);
        let sizing_b = self.b.sizing(env);
        if VERTICAL {
            Sizing::stack_y(&sizing_a, &sizing_b)
        } else {
            Sizing::stack_x(&sizing_a, &sizing_b)
        }
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        let shared = Rc::new(StackShared::<_, _, VERTICAL> {
            a: DynWidget::from_ref(&self.a),
            b: DynWidget::from_ref(&self.b),
            split: self.split,
            layout_cache: Cache::new(),
            slot,
        });
        StackInst {
            a: self
                .a
                .inst(env, StackSlot::<_, _, VERTICAL, false>(shared.clone())),
            b: self
                .b
                .inst(env, StackSlot::<_, _, VERTICAL, true>(shared.clone())),
            shared,
        }
    }
}

/// Contains the information that is shared between the two instances of a [`Stack`] widget.
struct StackShared<'a, Env: WidgetEnvironment + Track + ?Sized, S, const VERTICAL: bool> {
    // Use `DynWidget` here to prevent explosion of generic types that would otherwise occur.
    a: &'a DynWidget<'a, Env>,
    b: &'a DynWidget<'a, Env>,
    split: RationalU32,
    layout_cache: Cache<Env, StackLayout>,
    slot: S,
}

/// Describes the layout of a [`Stack`] widget at a given moment.
#[derive(Clone, Copy)]
struct StackLayout {
    size: Size2i,
    split: u32,
}

impl<'a, Env: WidgetEnvironment + Track + ?Sized, S: WidgetSlot<Env>, const VERTICAL: bool>
    StackShared<'a, Env, S, VERTICAL>
{
    /// Gets the current layout of the widget.
    pub fn layout(&self, env: &Env) -> StackLayout {
        self.layout_cache.get(env, |env, _| {
            let size = self.slot.size(env);
            let split = if VERTICAL {
                split_size_y(&self.a.sizing(env), &self.b.sizing(env), self.split, size)
            } else {
                split_size_x(&self.a.sizing(env), &self.b.sizing(env), self.split, size)
            };
            StackLayout { size, split }
        })
    }

    /// Determines which side of the widget a particular point is on.
    pub fn side(&self, env: &Env, pos: Vector2i) -> bool {
        let offset = pos - self.slot.min(env);
        let layout = self.layout(env);
        if VERTICAL {
            offset.y >= layout.split as i32
        } else {
            offset.x >= layout.split as i32
        }
    }
}

/// A slot for a [`Stack`] widget.
struct StackSlot<
    'a,
    Env: WidgetEnvironment + Track + ?Sized,
    S,
    const VERTICAL: bool,
    const SIDE: bool,
>(Rc<StackShared<'a, Env, S, VERTICAL>>);

impl<
        'a,
        Env: WidgetEnvironment + Track + ?Sized,
        S: WidgetSlot<Env>,
        const VERTICAL: bool,
        const SIDE: bool,
    > Clone for StackSlot<'a, Env, S, VERTICAL, SIDE>
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<
        'a,
        Env: WidgetEnvironment + Track + ?Sized,
        S: WidgetSlot<Env>,
        const VERTICAL: bool,
        const SIDE: bool,
    > WidgetSlot<Env> for StackSlot<'a, Env, S, VERTICAL, SIDE>
{
    fn is_visible(&self, env: &Env) -> bool {
        self.0.slot.is_visible(env)
    }

    fn size(&self, env: &Env) -> Size2i {
        let layout = self.0.layout(env);
        if SIDE {
            if VERTICAL {
                size2i(layout.size.x, layout.size.y - layout.split)
            } else {
                size2i(layout.size.x - layout.split, layout.size.y)
            }
        } else if VERTICAL {
            size2i(layout.size.x, layout.split)
        } else {
            size2i(layout.split, layout.size.y)
        }
    }

    fn min(&self, env: &Env) -> Point2i {
        self.0.slot.min(env)
            + if SIDE {
                if VERTICAL {
                    vec2i(0, self.0.layout(env).split as i32)
                } else {
                    vec2i(self.0.layout(env).split as i32, 0)
                }
            } else {
                vec2i(0, 0)
            }
    }

    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent) {
        self.0.slot.bubble_general_event(env, event)
    }
}

/// A [`WidgetInst`] for a [`Stack`] widget.
struct StackInst<'a, Env: WidgetEnvironment + Track + ?Sized, S, A, B, const VERTICAL: bool> {
    shared: Rc<StackShared<'a, Env, S, VERTICAL>>,
    a: A,
    b: B,
}

impl<
        Env: WidgetEnvironment + Track + ?Sized,
        S: WidgetSlot<Env>,
        A: WidgetInst<Env>,
        B: WidgetInst<Env>,
        const VERTICAL: bool,
    > WidgetInst<Env> for StackInst<'_, Env, S, A, B, VERTICAL>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        self.a.draw(env, drawer);
        self.b.draw(env, drawer);
    }

    fn hover_feedback(&self, env: &Env, pos: Vector2i, f: &mut dyn FnMut(&dyn Any)) -> bool {
        if self.shared.side(env, pos) {
            self.b.hover_feedback(env, pos, f)
        } else {
            self.a.hover_feedback(env, pos, f)
        }
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env> {
        if self.shared.side(env, pos) {
            self.b.cursor_event(env, pos, event)
        } else {
            self.a.cursor_event(env, pos, event)
        }
    }

    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>> {
        if backward {
            self.b
                .focus(env, backward)
                .or_else(|| self.a.focus(env, backward))
        } else {
            self.a
                .focus(env, backward)
                .or_else(|| self.b.focus(env, backward))
        }
    }
}

/// Determines the width allocated to `a`, given the target splitting ratio and the actual size
/// of a stack.
fn split_size_x(a: &Sizing, b: &Sizing, split: RationalU32, size: Size2i) -> u32 {
    let y = size.y;
    split_size(
        a,
        b,
        split,
        size.x,
        |s, max_x| s.upto_x(max_x, y),
        |s, min_x| s.downto_x(min_x, y),
    )
}

/// Determines the height allocated to `a`, given the target splitting ratio and the actual size
/// of a stack.
fn split_size_y(a: &Sizing, b: &Sizing, split: RationalU32, size: Size2i) -> u32 {
    let x = size.x;
    split_size(
        a,
        b,
        split,
        size.y,
        |s, max_y| s.upto_y(x, max_y),
        |s, min_y| s.downto_y(x, min_y),
    )
}

/// General function for determining the size allocation between the parts of a stack.
fn split_size(
    a: &Sizing,
    b: &Sizing,
    split: RationalU32,
    size: u32,
    upto: impl Fn(&Sizing, u32) -> Option<u32>,
    downto: impl Fn(&Sizing, u32) -> Option<u32>,
) -> u32 {
    let target = mul_rational(split, size);
    let mut small = target;
    let mut big = target;
    let mut small_solved = false;
    let mut big_solved = false;
    loop {
        if target - small <= big - target {
            if small_solved {
                return small;
            }
            if let Some(size_a) = upto(a, small) {
                if let Some(size_b) = downto(b, size - size_a) {
                    let n_small = size - size_b;
                    debug_assert!(n_small <= small);
                    small_solved = n_small == size_a;
                    small = n_small;
                    continue;
                }
            }
            loop {
                if big_solved {
                    return big;
                }
                if let Some(size_a) = downto(a, big) {
                    if let Some(size_b) = upto(b, size - size_a) {
                        let n_big = size - size_b;
                        debug_assert!(big <= n_big);
                        big_solved = n_big == size_a;
                        big = n_big;
                        continue;
                    }
                }
                panic!("invalid size");
            }
        } else {
            if big_solved {
                return big;
            }
            if let Some(size_a) = downto(a, big) {
                if let Some(size_b) = upto(b, size - size_a) {
                    let n_big = size - size_b;
                    debug_assert!(big <= n_big);
                    big_solved = n_big == size_a;
                    big = n_big;
                    continue;
                }
            }
            loop {
                if small_solved {
                    return small;
                }
                if let Some(size_a) = upto(a, small) {
                    if let Some(size_b) = downto(b, size - size_a) {
                        let n_small = size - size_b;
                        debug_assert!(n_small <= small);
                        small_solved = n_small == size_a;
                        small = n_small;
                        continue;
                    }
                }
                panic!("invalid size");
            }
        }
    }
}

/// Multiplies a [`RationalU32`] by a [`u32`] and rounds the result towards 0. This assumes that
/// `a` is at most 1.
fn mul_rational(a: RationalU32, b: u32) -> u32 {
    u32::try_from(u64::from(b) * u64::from(*a.numer()) / u64::from(*a.denom())).unwrap()
}
