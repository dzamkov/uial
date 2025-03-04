use crate::prelude::*;

/// Contains [`Restrict`]-related extension methods for [`Widget`].
pub trait RestrictWidgetExt: WidgetLike + Sized {
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
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given height.
    fn with_height(self, height: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: height - 1,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: height - 1,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given minimum width.
    fn with_min_width(self, width: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given minimum height.
    fn with_min_height(self, height: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: height - 1,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given maximum width.
    fn with_max_width(self, width: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given maximum height.
    fn with_max_height(self, height: u32) -> Restrict<Self, Const<Sizing>> {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: height - 1,
            },
        ))
    }
}

impl<T: WidgetLike> RestrictWidgetExt for T {}

/// A wrapper over a [`Widget`] which restricts the possible sizes it can be instantiated with.
#[derive(Clone, Copy)]
pub struct Restrict<T: WidgetLike, S: PropertyBase<Value = Sizing>> {
    inner: T,
    sizing: S,
}

impl<T: WidgetLike, S: PropertyBase<Value = Sizing>> Restrict<T, S> {
    /// Constructs a new [`Restrict`] widget.
    pub fn new(inner: T, sizing: S) -> Self {
        Self { inner, sizing }
    }
}

impl<T: WidgetLike> Restrict<T, Const<Sizing>> {
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
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given height.
    pub fn with_height(self, height: u32) -> Self {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: height - 1,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: height - 1,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given minimum width.
    pub fn with_min_width(self, width: u32) -> Self {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given minimum height.
    pub fn with_min_height(self, height: u32) -> Self {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: height - 1,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given maximum width.
    pub fn with_max_width(self, width: u32) -> Self {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: width - 1,
                y_minus_1: u32::MAX,
            },
        ))
    }

    /// Restricts this [`Widget`] to the given maximum height.
    pub fn with_max_height(self, height: u32) -> Self {
        self.with_sizing(Sizing::range(
            Size2i {
                x_minus_1: 0,
                y_minus_1: 0,
            },
            Size2i {
                x_minus_1: u32::MAX,
                y_minus_1: height - 1,
            },
        ))
    }
}

impl<T: WidgetLike, S: PropertyBase<Value = Sizing>> WidgetLike for Restrict<T, S> {}

impl<Env: WidgetEnvironment + ?Sized, T: IntoWidget<Env>, S: Property<Env, Value = Sizing>>
    IntoWidget<Env> for Restrict<T, S>
{
    fn into_widget(self, env: &Env) -> impl Widget<Env> {
        Restrict {
            inner: self.inner.into_widget(env),
            sizing: self.sizing,
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

    fn place<'a, Slot: WidgetSlot<Env> + 'a>(
        &'a self,
        env: &Env,
        slot: Slot,
    ) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        self.inner.place(env, slot)
    }
}
