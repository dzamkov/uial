use crate::prelude::*;

/// Contains [`Pad`]-related extension methods for [`Widget`].
pub trait PadWidgetExt: WidgetLike + Sized {
    /// Surrounds this [`Widget`] with the given amount of padding.
    fn with_padding(self, amount: Padding2i) -> Pad<Self> {
        Pad::new(self, amount)
    }
}

impl<T: WidgetLike> PadWidgetExt for T {}

/// A wrapper over a [`Widget`] which surroundings it with a specified amount of padding.
#[derive(Clone, Copy)]
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

impl<T: WidgetLike> WidgetLike for Pad<T> {}

impl<Env: WidgetEnvironment + ?Sized, T: IntoWidget<Env>> IntoWidget<Env> for Pad<T> {
    fn into_widget(self, env: &Env) -> impl Widget<Env> {
        Pad {
            inner: self.inner.into_widget(env),
            amount: self.amount,
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>> Widget<Env> for Pad<T> {
    fn sizing(&self, env: &Env) -> Sizing {
        let mut res = self.inner.sizing(env);
        res.add_assign(
            self.amount.n_x + self.amount.p_x,
            self.amount.n_y + self.amount.p_y,
        );
        res
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(
        &'a self,
        env: &Env,
        slot: S,
    ) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        self.inner.place(
            env,
            PadSlot {
                padding: &self.amount,
                source: slot,
            },
        )
    }
}

/// A [`WidgetSlot`] provided by a [`Pad`] widget to its inner widget.
#[derive(Clone)]
struct PadSlot<'a, S> {
    padding: &'a Padding2i,
    source: S,
}

impl<Env: WidgetEnvironment + ?Sized, S: WidgetSlot<Env>> WidgetSlot<Env> for PadSlot<'_, S> {
    fn is_visible(&self, env: &Env) -> bool {
        self.source.is_visible(env)
    }

    fn size(&self, env: &Env) -> Size2i {
        let mut res = self.source.size(env);
        res.x_minus_1 -= self.padding.n_x + self.padding.p_x;
        res.y_minus_1 -= self.padding.n_y + self.padding.p_y;
        res
    }

    fn min(&self, env: &Env) -> Point2i {
        self.source.min(env) + vec2i(self.padding.n_x as i32, self.padding.n_y as i32)
    }

    // TODO: Implement `bounds`

    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent) {
        self.source.bubble_general_event(env, event)
    }
}
