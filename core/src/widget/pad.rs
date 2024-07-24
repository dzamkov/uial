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

impl<T: WidgetBase> WidgetBase for Pad<T> {}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>> Widget<Env> for Pad<T> {
    fn sizing(&self, env: &Env) -> Sizing {
        let mut res = self.inner.sizing(env);
        res += self.amount.size();
        res
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        self.inner.inst(
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
        self.source.size(env) - self.padding.size()
    }

    fn min(&self, env: &Env) -> Point2i {
        self.source.min(env) + vec2i(self.padding.n_x as i32, self.padding.n_y as i32)
    }

    // TODO: Implement `bounds`

    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent) {
        self.source.bubble_general_event(env, event)
    }
}
