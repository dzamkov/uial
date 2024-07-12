use super::*;

/// Contains [`Restrict`]-related extension methods for [`Widget`].
pub trait RestrictWidgetExt: WidgetBase + Sized {
    /// Restricts this [`Widget`] to the sizes allowed by the given [`Sizing`].
    fn with_sizing(self, sizing: Sizing) -> Restrict<Self, Const<Sizing>> {
        Restrict::new(self, Const(sizing))
    }

    /// Restricts this [`Widget`] to the given size.
    fn with_size(self, size: Size2i) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::exact(size))
    }

    /// Restricts this [`Widget`] to the given width.
    fn with_width(self, width: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(size2i(width, 0), size2i(width, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given height.
    fn with_height(self, height: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(size2i(0, height), size2i(u32::MAX, height)))
    }

    /// Restricts this [`Widget`] to the given minimum width.
    fn with_min_width(self, width: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(size2i(width, 0), size2i(u32::MAX, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given minimum height.
    fn with_min_height(self, height: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(size2i(0, height), size2i(u32::MAX, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given maximum width.
    fn with_max_width(self, width: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(size2i(0, 0), size2i(width, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given maximum height.
    fn with_max_height(self, height: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(size2i(0, 0), size2i(u32::MAX, height)))
    }
}

impl<T: WidgetBase> RestrictWidgetExt for T {}

/// A wrapper over a [`Widget`] which restricts the possible sizes it can be instantiated with.
pub struct Restrict<T: WidgetBase, S: PropertyBase<Value = Sizing>> {
    inner: T,
    sizing: S,
}

impl<T: WidgetBase, S: PropertyBase<Value = Sizing>> Restrict<T, S> {
    /// Constructs a new [`Restrict`] widget.
    pub fn new(inner: T, sizing: S) -> Self {
        Self { inner, sizing }
    }
}

impl<T: WidgetBase> Restrict<T, Const<Sizing>> {
    /// Restricts this [`Widget`] to the sizes allowed by the given [`Sizing`].
    pub fn with_sizing(mut self, sizing: Sizing) -> Self {
        self.sizing.0 = &self.sizing.0 & &sizing;
        self
    }

    /// Restricts this [`Widget`] to the given size.
    pub fn with_size(self, size: Size2i) -> Self {
        self.with_sizing(Sizing::exact(size))
    }

    /// Restricts this [`Widget`] to the given width.
    pub fn with_width(self, width: u32) -> Self {
        self.with_sizing(Sizing::range(size2i(width, 0), size2i(width, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given height.
    pub fn with_height(self, height: u32) -> Self {
        self.with_sizing(Sizing::range(size2i(0, height), size2i(u32::MAX, height)))
    }

    /// Restricts this [`Widget`] to the given minimum width.
    pub fn with_min_width(self, width: u32) -> Self {
        self.with_sizing(Sizing::range(size2i(width, 0), size2i(u32::MAX, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given minimum height.
    pub fn with_min_height(self, height: u32) -> Self {
        self.with_sizing(Sizing::range(size2i(0, height), size2i(u32::MAX, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given maximum width.
    pub fn with_max_width(self, width: u32) -> Self {
        self.with_sizing(Sizing::range(size2i(0, 0), size2i(width, u32::MAX)))
    }

    /// Restricts this [`Widget`] to the given maximum height.
    pub fn with_max_height(self, height: u32) -> Self {
        self.with_sizing(Sizing::range(size2i(0, 0), size2i(u32::MAX, height)))
    }
}

impl<T: WidgetBase, S: PropertyBase<Value = Sizing>> WidgetBase for Restrict<T, S> {
    type Layout = T::Layout;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        self.inner.size(layout)
    }
}

impl<T: WidgetBase, S: PropertyBase<Value = Sizing>> WidgetInner for Restrict<T, S> {
    type Inner = T;
    fn inner<'a, 'b>(inst: WidgetInst<'a, 'b, Self>) -> WidgetInst<'a, 'b, Self::Inner> {
        WidgetInst {
            widget: &inst.widget.inner,
            min: inst.min,
            layout: inst.layout,
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>, S: Property<Env, Value = Sizing>> Widget<Env>
    for Restrict<T, S>
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.sizing
            .with_ref(env, |sizing| &self.inner.sizing(env) & sizing)
    }

    fn layout(&self, env: &Env, size: Size2i) -> <T::Layout as WidgetLayout>::Owned {
        self.inner.layout(env, size)
    }

    fn relayout(&self, layout: &mut T::Layout, env: &Env, size: Size2i) {
        self.inner.relayout(layout, env, size)
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
