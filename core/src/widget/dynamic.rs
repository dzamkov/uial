use crate::prelude::*;
use std::rc::Rc;

/// A [`Widget`] of an unspecified type, determined at runtime.
#[repr(transparent)]
pub struct DynWidget<'a, Env: WidgetEnvironment + ?Sized>(dyn WidgetSafe<Env> + 'a);

impl<'a, Env: WidgetEnvironment + ?Sized> DynWidget<'a, Env> {
    /// Converts a reference to a [`Widget`] into a reference to a [`DynWidget`].
    pub fn from_ref<W: Widget<Env> + 'a>(source: &W) -> &Self {
        let source: &(dyn WidgetSafe<Env> + 'a) = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts a boxed [`Widget`] into a boxed [`DynWidget`].
    pub fn from_box<W: Widget<Env> + 'a>(source: Box<W>) -> Box<Self> {
        let source: Box<dyn WidgetSafe<Env> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts an [`Rc`]-wrapped [`Widget`] into a boxed [`DynWidget`].
    pub fn from_rc<W: Widget<Env> + 'a>(source: Rc<W>) -> Rc<Self> {
        let source: Rc<dyn WidgetSafe<Env> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }
}

/// An object-safe variant of [`Widget`].
trait WidgetSafe<Env: WidgetEnvironment + ?Sized> {
    fn sizing(&self, env: &Env) -> Sizing;
    fn inst<'a>(
        &'a self,
        env: &Env,
        slot: Rc<dyn WidgetSlotSafe<Env> + 'a>,
    ) -> Rc<dyn WidgetInst<Env> + 'a>
    where
        Env: 'a;
}

impl<Env: WidgetEnvironment + ?Sized, W: Widget<Env>> WidgetSafe<Env> for W {
    fn sizing(&self, env: &Env) -> Sizing {
        self.sizing(env)
    }

    fn inst<'a>(
        &'a self,
        env: &Env,
        slot: Rc<dyn WidgetSlotSafe<Env> + 'a>,
    ) -> Rc<dyn WidgetInst<Env> + 'a>
    where
        Env: 'a,
    {
        self.inst(env, slot).into_rc_dyn()
    }
}

/// An object-safe variant of [`WidgetSlot`].
trait WidgetSlotSafe<Env: WidgetEnvironment + ?Sized> {
    fn is_visible(&self, env: &Env) -> bool;
    fn size(&self, env: &Env) -> Size2i;
    fn min(&self, env: &Env) -> Point2i;
    fn bounds(&self, env: &Env) -> Box2i;
    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent);
}

impl<Env: WidgetEnvironment + ?Sized, S: WidgetSlot<Env>> WidgetSlotSafe<Env> for S {
    fn is_visible(&self, env: &Env) -> bool {
        self.is_visible(env)
    }

    fn size(&self, env: &Env) -> Size2i {
        self.size(env)
    }

    fn min(&self, env: &Env) -> Point2i {
        self.min(env)
    }

    fn bounds(&self, env: &Env) -> Box2i {
        self.bounds(env)
    }

    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent) {
        self.bubble_general_event(env, event)
    }
}

impl<Env: WidgetEnvironment + ?Sized> WidgetSlot<Env> for Rc<dyn WidgetSlotSafe<Env> + '_> {
    fn is_visible(&self, env: &Env) -> bool {
        (**self).is_visible(env)
    }

    fn size(&self, env: &Env) -> Size2i {
        (**self).size(env)
    }

    fn min(&self, env: &Env) -> Point2i {
        (**self).min(env)
    }

    fn bounds(&self, env: &Env) -> Box2i {
        (**self).bounds(env)
    }

    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent) {
        (**self).bubble_general_event(env, event)
    }
}

impl<Env: WidgetEnvironment + ?Sized> WidgetBase for DynWidget<'_, Env> {}

impl<'a, Env: WidgetEnvironment + ?Sized + 'a> Widget<Env> for DynWidget<'a, Env> {
    fn sizing(&self, env: &Env) -> Sizing {
        self.0.sizing(env)
    }

    fn inst<'b, S: WidgetSlot<Env> + 'b>(&'b self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'b
    where
        Env: 'b,
    {
        self.0.inst(env, Rc::new(slot))
    }
}
