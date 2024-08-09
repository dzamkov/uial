use crate::prelude::*;

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

impl<T: WidgetBase, S: PropertyBase<Value = Sizing>> WidgetBase for Restrict<T, S> {}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>, S: Property<Env, Value = Sizing>> Widget<Env>
    for Restrict<T, S>
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.sizing
            .with_ref(env, |sizing| &self.inner.sizing(env) & sizing)
    }

    fn inst<'a, Slot: WidgetSlot<Env> + 'a>(
        &'a self,
        env: &Env,
        slot: Slot,
    ) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        self.inner.inst(env, slot)
    }
}
