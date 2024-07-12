use super::*;

/// Contains [`Pad`]-related extension methods for [`Widget`].
pub trait PadWidgetExt: WidgetBase + Sized {
    /// Surrounds this [`Widget`] with the given amount of padding.
    fn with_padding(self, amount: Padding2i) -> Pad<Self> {
        Pad::new(self, amount)
    }
}

impl<T: WidgetBase> PadWidgetExt for T {}

/// A wrapper over a [`Widget`] which surroundings it with a specified amount of padding.
pub struct Pad<T> {
    inner: T,
    amount: Padding2i,
}

impl<T> Pad<T> {
    /// Constructs a new [`Pad`] widget.
    pub fn new(inner: T, amount: Padding2i) -> Self {
        Self { inner, amount }
    }
}

impl<T: WidgetBase> WidgetBase for Pad<T> {
    type Layout = T::Layout;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        self.inner.size(layout) + self.amount.size()
    }
}

impl<T: WidgetBase> WidgetInner for Pad<T> {
    type Inner = T;
    fn inner<'a, 'b>(inst: WidgetInst<'a, 'b, Self>) -> WidgetInst<'a, 'b, Self::Inner> {
        WidgetInst {
            widget: &inst.widget.inner,
            min: inst.min + vec2i(inst.widget.amount.n_x as i32, inst.widget.amount.n_y as i32),
            layout: inst.layout,
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>> Widget<Env> for Pad<T> {
    fn sizing(&self, env: &Env) -> Sizing {
        let mut res = self.inner.sizing(env);
        res += self.amount.size();
        res
    }

    fn layout(&self, env: &Env, size: Size2i) -> <T::Layout as WidgetLayout>::Owned {
        self.inner.layout(env, size - self.amount.size())
    }

    fn relayout(&self, layout: &mut T::Layout, env: &Env, size: Size2i) {
        self.inner.relayout(layout, env, size - self.amount.size())
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
