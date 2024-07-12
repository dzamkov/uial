use super::*;

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

/// The layout for an [`Stack`] widget.
pub struct StackLayout<A: WidgetLayout + ?Sized, B: WidgetLayout + ?Sized> {
    a: A::Owned,
    b: B::Owned,
}

impl<A: WidgetBase, B: WidgetBase, const VERTICAL: bool> WidgetBase for Stack<A, B, VERTICAL> {
    type Layout = StackLayout<A::Layout, B::Layout>;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        let size_a = self.a.size(layout.a.borrow());
        let size_b = self.b.size(layout.b.borrow());
        if VERTICAL {
            debug_assert_eq!(size_a.x, size_b.x);
            size2i(size_a.x, size_a.y + size_b.y)
        } else {
            debug_assert_eq!(size_a.y, size_b.y);
            size2i(size_a.x + size_b.x, size_a.y)
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, A: Widget<Env>, B: Widget<Env>, const VERTICAL: bool>
    Widget<Env> for Stack<A, B, VERTICAL>
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

    fn layout(&self, env: &Env, size: Size2i) -> Self::Layout {
        let sizing_a = self.a.sizing(env);
        let sizing_b = self.b.sizing(env);
        if VERTICAL {
            let size_y_a = split_size_y(&sizing_a, &sizing_b, self.split, size);
            StackLayout {
                a: self.a.layout(env, size2i(size.x, size_y_a)),
                b: self.b.layout(env, size2i(size.x, size.y - size_y_a)),
            }
        } else {
            let size_x_a = split_size_x(&sizing_a, &sizing_b, self.split, size);
            StackLayout {
                a: self.a.layout(env, size2i(size_x_a, size.y)),
                b: self.b.layout(env, size2i(size.x - size_x_a, size.y)),
            }
        }
    }

    #[rustfmt::skip]
    fn relayout(&self, layout: &mut Self::Layout, env: &Env, size: Size2i) {
        let sizing_a = self.a.sizing(env);
        let sizing_b = self.b.sizing(env);
        if VERTICAL {
            let size_y_a = split_size_y(&sizing_a, &sizing_b, self.split, size);
            self.a.relayout(layout.a.borrow_mut(), env, size2i(size.x, size_y_a));
            self.b.relayout(layout.b.borrow_mut(), env, size2i(size.x, size.y - size_y_a));
        } else {
            let size_x_a = split_size_x(&sizing_a, &sizing_b, self.split, size);
            self.a.relayout(layout.a.borrow_mut(), env, size2i(size_x_a, size.y));
            self.b.relayout(layout.b.borrow_mut(), env, size2i(size.x - size_x_a, size.y));
        }
    }

    fn outline<'a>(
        inst: WidgetInst<'a, 'a, Self>,
        outliner: &mut (impl WidgetOutliner<'a, Env> + ?Sized),
    ) {
        inner_a(inst).outline(outliner);
        inner_b(inst).outline(outliner);
    }

    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer) {
        inner_a(inst).draw(env, drawer);
        inner_b(inst).draw(env, drawer);
    }

    fn hover_interactions<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &Env,
        cursor: impl Cursor<'a, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        if side(inst, cursor.pos(env)) {
            inner_b(inst).hover_interactions(env, cursor, f)
        } else {
            inner_a(inst).hover_interactions(env, cursor, f)
        }
    }

    fn mouse_scroll<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus {
        if side(inst, cursor.pos(env)) {
            inner_b(inst).mouse_scroll(env, cursor, amount)
        } else {
            inner_a(inst).mouse_scroll(env, cursor, amount)
        }
    }

    fn mouse_down<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus {
        if side(inst, cursor.pos(env)) {
            inner_b(inst).mouse_down(env, cursor, button)
        } else {
            inner_a(inst).mouse_down(env, cursor, button)
        }
    }

    fn focus<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        keyboard: impl Keyboard<'a, Env> + Clone,
        backward: bool,
    ) -> EventStatus {
        if backward ^ VERTICAL {
            if let EventStatus::Handled = inner_b(inst).focus(env, keyboard.clone(), backward) {
                return EventStatus::Handled;
            }
            inner_a(inst).focus(env, keyboard, backward)
        } else {
            if let EventStatus::Handled = inner_a(inst).focus(env, keyboard.clone(), backward) {
                return EventStatus::Handled;
            }
            inner_b(inst).focus(env, keyboard, backward)
        }
    }
}

/// Gets the [`WidgetInst`] for the first inner widget of a [`Stack`] widget.
fn inner_a<'a, 'b, A: WidgetBase, B: WidgetBase, const VERTICAL: bool>(
    inst: WidgetInst<'a, 'b, Stack<A, B, VERTICAL>>,
) -> WidgetInst<'a, 'b, A> {
    WidgetInst {
        widget: &inst.widget.a,
        min: inst.min,
        layout: inst.layout.a.borrow(),
    }
}

/// Gets the [`WidgetInst`] for the second inner widget of a [`Stack`] widget.
fn inner_b<'a, 'b, A: WidgetBase, B: WidgetBase, const VERTICAL: bool>(
    inst: WidgetInst<'a, 'b, Stack<A, B, VERTICAL>>,
) -> WidgetInst<'a, 'b, B> {
    let size_a = inst.widget.a.size(inst.layout.a.borrow());
    WidgetInst {
        widget: &inst.widget.b,
        min: inst.min
            + if VERTICAL {
                vec2i(0, size_a.y as i32)
            } else {
                vec2i(size_a.x as i32, 0)
            },
        layout: inst.layout.b.borrow(),
    }
}

/// Determines which side of a [`Stack`] widget instance a particular point is on.
fn side<A: WidgetBase, B: WidgetBase, const VERTICAL: bool>(
    inst: WidgetInst<Stack<A, B, VERTICAL>>,
    pos: Vector2i
) -> bool {
    let size_a = inst.widget.a.size(inst.layout.a.borrow());
    if VERTICAL {
        pos.y >= inst.min.y + size_a.y as i32
    } else {
        pos.x >= inst.min.x + size_a.x as i32
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
