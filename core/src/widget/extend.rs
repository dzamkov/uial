use crate::prelude::*;
use crate::RationalU32;

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

impl<T: WidgetBase, const VERTICAL: bool> WidgetBase for Extend<T, VERTICAL> {}

impl<Env: WidgetEnvironment + Track + ?Sized, T: Widget<Env>, const VERTICAL: bool> Widget<Env>
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

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        self.inner.inst(
            env,
            ExtendSlot {
                widget: self,
                layout_cache: Cache::new(),
                source: slot,
            },
        )
    }
}

/// A [`WidgetSlot`] provided by an [`Extend`] widget to its inner widget.
struct ExtendSlot<'a, Env: Track + ?Sized, T, S, const VERTICAL: bool> {
    widget: &'a Extend<T, VERTICAL>,
    layout_cache: Cache<Env, ExtendLayout>,
    source: S,
}

impl<Env: Track + ?Sized, T, S: Clone, const VERTICAL: bool> Clone
    for ExtendSlot<'_, Env, T, S, VERTICAL>
{
    fn clone(&self) -> Self {
        Self {
            widget: self.widget,
            layout_cache: Cache::new(),
            source: self.source.clone(),
        }
    }
}

/// Describes the internal layout of an [`Extend`] widget instance.
#[derive(Clone, Copy)]
struct ExtendLayout {
    before_padding: u32,
    inner_size: Size2i,
}

impl<
        Env: WidgetEnvironment + Track + ?Sized,
        T: Widget<Env>,
        S: WidgetSlot<Env>,
        const VERTICAL: bool,
    > ExtendSlot<'_, Env, T, S, VERTICAL>
{
    /// Gets the internal layout associated with this slot.
    pub fn layout(&self, env: &Env) -> ExtendLayout {
        self.layout_cache.get(env, |env, _| {
            let inner_sizing = self.widget.inner.sizing(env);
            let align = self.widget.align;
            let size = self.source.size(env);
            if VERTICAL {
                let inner_size_y = inner_sizing.upto_y(size.x, size.y);
                let inner_size_y = inner_size_y.expect("invalid size");
                let padding = size.y - inner_size_y;
                let before_padding = mul_rational(align, padding);
                ExtendLayout {
                    before_padding,
                    inner_size: size2i(size.x, inner_size_y),
                }
            } else {
                let inner_size_x = inner_sizing.upto_x(size.x, size.y);
                let inner_size_x = inner_size_x.expect("invalid size");
                let padding = size.x - inner_size_x;
                let before_padding = mul_rational(align, padding);
                ExtendLayout {
                    before_padding,
                    inner_size: size2i(inner_size_x, size.y),
                }
            }
        })
    }
}

impl<
        Env: WidgetEnvironment + Track + ?Sized,
        T: Widget<Env>,
        S: WidgetSlot<Env>,
        const VERTICAL: bool,
    > WidgetSlot<Env> for ExtendSlot<'_, Env, T, S, VERTICAL>
{
    fn is_visible(&self, env: &Env) -> bool {
        self.source.is_visible(env)
    }

    fn size(&self, env: &Env) -> Size2i {
        self.layout(env).inner_size
    }

    fn min(&self, env: &Env) -> Point2i {
        let before_padding = self.layout(env).before_padding;
        self.source.min(env)
            + if VERTICAL {
                vec2i(0, before_padding as i32)
            } else {
                vec2i(before_padding as i32, 0)
            }
    }

    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent) {
        self.source.bubble_general_event(env, event)
    }
}

/// Multiplies a [`RationalU32`] by a [`u32`] and rounds the result towards 0. This assumes that
/// `a` is at most 1.
fn mul_rational(a: RationalU32, b: u32) -> u32 {
    u32::try_from(u64::from(b) * u64::from(*a.numer()) / u64::from(*a.denom())).unwrap()
}
