use crate::prelude::*;

/// Contains [`Minimize`]-related extension methods for [`Widget`].
pub trait MinimizeWidgetExt: WidgetBase + Sized {
    /// Forces the size of this [`Widget`] to be minimized along the X axis.
    fn minimize_h(self) -> MinimizeH<Self> {
        Minimize { inner: self }
    }

    /// Forces the size of this [`Widget`] to be minimized along the Y axis.
    fn minimize_v(self) -> MinimizeV<Self> {
        Minimize { inner: self }
    }

    /// Forces the size of this [`Widget`] to be minimized.
    fn minimize(self) -> MinimizeV<MinimizeH<Self>> {
        // TODO: This is not entirely correct. e.g. a widget whose sizing requires its width
        // to equal its height will not be properly minimized.
        self.minimize_h().minimize_v()
    }
}

impl<T: WidgetBase> MinimizeWidgetExt for T {}

/// A wrapper over a [`Widget`] which forces its size to be minimized along the X axis.
pub type MinimizeH<T> = Minimize<T, false>;

/// A wrapper over a [`Widget`] which forces its size to be minimized along the Y axis.
pub type MinimizeV<T> = Minimize<T, true>;

/// A wrapper over a [`Widget`] which forces its size to be minimized along some axis.
pub struct Minimize<T, const VERTICAL: bool> {
    inner: T,
}

impl<T: WidgetBase, const VERTICAL: bool> WidgetBase for Minimize<T, VERTICAL> {}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>, const VERTICAL: bool> Widget<Env>
    for Minimize<T, VERTICAL>
{
    fn sizing(&self, env: &Env) -> Sizing {
        let inner_sizing = self.inner.sizing(env);
        if VERTICAL {
            inner_sizing.minimize_y()
        } else {
            inner_sizing.minimize_x()
        }
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        self.inner.inst(env, slot)
    }
}
