use super::*;

/// Contains [`Extend`]-related extension methods for [`Widget`].
pub trait ExtendWidgetExt: WidgetBase + Sized {
    /// Applies a variable amount of padding to the left and right side of this [`Widget`].
    /// The ratio of the amount of left (-X) padding to the total amount of padding is specified by
    /// `align`.
    fn extend_h(self, align: RationalU32) -> ExtendH<Self> {
        Extend::new(self, align)
    }

    /// Applies a variable amount of padding to the bottom and top side of this [`Widget`].
    /// The ratio of the amount of bottom padding to the total amount of padding is specified by
    /// `align`.
    fn extend_v(self, align: RationalU32) -> ExtendV<Self> {
        Extend::new(self, align)
    }

    /// Horizontally and vertically centers this [`Widget`] by surronding it with a variable
    /// and (approximately) uniform amount of padding.
    fn center(self) -> ExtendV<ExtendH<Self>> {
        self.center_h().center_v()
    }

    /// Horizontally centers this [`Widget`] by applying a variable and (approximately) equal
    /// amount of padding to its left and right side.
    fn center_h(self) -> ExtendH<Self> {
        self.extend_h(RationalU32::new(1, 2))
    }

    /// Vertically centers this [`Widget`] by applying a variable and (approximately) equal
    /// amount of padding to its bottom (-Y) and top side.
    fn center_v(self) -> ExtendV<Self> {
        self.extend_v(RationalU32::new(1, 2))
    }
}

impl<T: WidgetBase> ExtendWidgetExt for T {}

/// A wrapper over a [`Widget`] which extends it horizontally or vertically by applying a
/// variable amount of padding on either side.
pub struct Extend<T, const VERTICAL: bool> {
    inner: T,
    align: RationalU32,
}

/// A wrapper over a [`Widget`] which extends it horizontally by applying a variable amount of
/// padding on its left and right side.
pub type ExtendH<T> = Extend<T, false>;

/// A wrapper over a [`Widget`] which extends it vertically by applying a variable amount of
/// padding on its bottom and top side.
pub type ExtendV<T> = Extend<T, true>;

impl<T, const VERTICAL: bool> Extend<T, VERTICAL> {
    /// Constructs a new [`Extend`] widget.
    pub fn new(inner: T, align: RationalU32) -> Self {
        assert!(
            align <= 1.into(),
            "`align` must be at most 1. `align` = {}",
            align
        );
        Self { inner, align }
    }
}

/// The layout for an [`Extend`] widget.
pub struct ExtendLayout<T: WidgetLayout + ?Sized> {
    pub before_padding: u32,
    pub after_padding: u32,
    pub inner: T::Owned,
}

impl<T: WidgetBase, const VERTICAL: bool> WidgetBase for Extend<T, VERTICAL> {
    type Layout = ExtendLayout<T::Layout>;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        let mut size = self.inner.size(layout.inner.borrow());
        let padding = layout.before_padding + layout.after_padding;
        if VERTICAL {
            size.y += padding;
        } else {
            size.x += padding;
        }
        size
    }
}

impl<T: WidgetBase, const VERTICAL: bool> WidgetInner for Extend<T, VERTICAL> {
    type Inner = T;
    fn inner<'a, 'b>(inst: WidgetInst<'a, 'b, Self>) -> WidgetInst<'a, 'b, Self::Inner> {
        WidgetInst {
            widget: &inst.widget.inner,
            min: inst.min
                + if VERTICAL {
                    vec2i(0, inst.layout.before_padding as i32)
                } else {
                    vec2i(inst.layout.before_padding as i32, 0)
                },
            layout: inst.layout.inner.borrow(),
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>, const VERTICAL: bool> Widget<Env>
    for Extend<T, VERTICAL>
{
    fn sizing(&self, env: &Env) -> Sizing {
        let inner_sizing = self.inner.sizing(env);
        if VERTICAL {
            inner_sizing.extend_y()
        } else {
            inner_sizing.extend_x()
        }
    }

    fn layout(&self, env: &Env, size: Size2i) -> ExtendLayout<T::Layout> {
        if VERTICAL {
            let inner_size_y = self.inner.sizing(env).upto_y(size.x, size.y);
            let inner_size_y = inner_size_y.expect("invalid size");
            let padding = size.y - inner_size_y;
            let before_padding = mul_rational(self.align, padding);
            let after_padding = padding - before_padding;
            ExtendLayout {
                before_padding,
                after_padding,
                inner: self.inner.layout(env, size2i(size.x, inner_size_y)),
            }
        } else {
            let inner_size_x = self.inner.sizing(env).upto_x(size.x, size.y);
            let inner_size_x = inner_size_x.expect("invalid size");
            let padding = size.x - inner_size_x;
            let before_padding = mul_rational(self.align, padding);
            let after_padding = padding - before_padding;
            ExtendLayout {
                before_padding,
                after_padding,
                inner: self.inner.layout(env, size2i(inner_size_x, size.y)),
            }
        }
    }

    fn relayout(&self, layout: &mut ExtendLayout<T::Layout>, env: &Env, size: Size2i) {
        if VERTICAL {
            let inner_size_y = self.inner.sizing(env).upto_y(size.x, size.y);
            let inner_size_y = inner_size_y.expect("invalid size");
            let padding = size.y - inner_size_y;
            layout.before_padding = mul_rational(self.align, padding);
            layout.after_padding = padding - layout.before_padding;
            self.inner.relayout(layout.inner.borrow_mut(), env, size2i(size.x, inner_size_y));
        } else {
            let inner_size_x = self.inner.sizing(env).upto_x(size.x, size.y);
            let inner_size_x = inner_size_x.expect("invalid size");
            let padding = size.x - inner_size_x;
            layout.before_padding = mul_rational(self.align, padding);
            layout.after_padding = padding - layout.before_padding;
            self.inner.relayout(layout.inner.borrow_mut(), env, size2i(inner_size_x, size.y));
        }
    }

    fn outline<'a>(
        inst: WidgetInst<'a, 'a, Self>,
        outliner: &mut (impl WidgetOutliner<'a, Env> + ?Sized),
    ) {
        inst.inner().outline(outliner)
    }

    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer) {
        inst.inner().draw(env, drawer)
    }

    fn hover_interactions<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &Env,
        cursor: impl Cursor<'a, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        inst.inner().hover_interactions(env, cursor, f)
    }

    fn mouse_scroll<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus {
        inst.inner().mouse_scroll(env, cursor, amount)
    }

    fn mouse_down<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus {
        inst.inner().mouse_down(env, cursor, button)
    }

    fn focus<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        keyboard: impl Keyboard<'a, Env> + Clone,
        backward: bool,
    ) -> EventStatus {
        inst.inner().focus(env, keyboard, backward)
    }
}

/// Multiplies a [`RationalU32`] by a [`u32`] and rounds the result towards 0. This assumes that
/// `a` is at most 1.
fn mul_rational(a: RationalU32, b: u32) -> u32 {
    u32::try_from(u64::from(b) * u64::from(*a.numer()) / u64::from(*a.denom())).unwrap()
}
