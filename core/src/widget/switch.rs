use super::*;
use std::cell::UnsafeCell;
use std::mem::ManuallyDrop;
use std::rc::Rc;

/// Contains [`Switch`]-related extension methods for [`Property`]s.
pub trait SwitchWidgetExt<T: WidgetBase + ?Sized>: PropertyBase<Value = Rc<T>> + Sized {
    /// Constructs a [`Widget`] which reflects the layout and appearance of the [`Widget`] inside
    /// this property.
    fn switch(self) -> Switch<Self, T> {
        Switch(self)
    }
}

impl<P: PropertyBase<Value = Rc<T>>, T: WidgetBase + ?Sized> SwitchWidgetExt<T> for P {}

/// A [`Widget`] defined by a [`Property`] which produces a widget. When the value of the property
/// changes, the widget will completely change its layout and appearance to reflect the new value.
pub struct Switch<P: PropertyBase<Value = Rc<T>>, T: WidgetBase + ?Sized>(P);

impl<P: PropertyBase<Value = Rc<T>>, T: WidgetBase + ?Sized> WidgetBase for Switch<P, T> {}

impl<P: Property<Env, Value = Rc<T>>, T: Widget<Env> + ?Sized, Env: WidgetEnvironment + ?Sized>
    Widget<Env> for Switch<P, T>
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.0.get(env).sizing(env)
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        SwitchInst {
            widget: self,
            cache: UnsafeCell::new(None),
            slot: Rc::new(slot),
        }
    }
}

/// An instance of a [`Switch`] widget.
struct SwitchInst<
    'a,
    P: Property<Env, Value = Rc<T>>,
    T: Widget<Env> + ?Sized,
    Env: WidgetEnvironment + ?Sized,
    Slot,
> {
    widget: &'a Switch<P, T>,
    // TODO: Once we are able to refer to the return type of `Widget::inst`, we shouldn't need
    // to use `dyn` anymore.
    #[allow(clippy::type_complexity)]
    cache: UnsafeCell<Option<Rc<Dependent<T, dyn WidgetInst<Env> + 'a>>>>,
    slot: Rc<Slot>,
}

/// Stores a value of type `Front` which may contain references into a backing `Rc<Back>`.
struct Dependent<Back: ?Sized, Front: ?Sized> {
    back: ManuallyDrop<Rc<Back>>,
    front: ManuallyDrop<Front>,
}

impl<Back: ?Sized, Front: ?Sized> Drop for Dependent<Back, Front> {
    fn drop(&mut self) {
        unsafe {
            // `front` must be dropped before `back`
            ManuallyDrop::drop(&mut self.front);
            ManuallyDrop::drop(&mut self.back);
        }
    }
}

impl<
        'a,
        P: Property<Env, Value = Rc<T>>,
        T: Widget<Env> + ?Sized,
        Env: WidgetEnvironment + ?Sized,
        Slot: WidgetSlot<Env>,
    > SwitchInst<'a, P, T, Env, Slot>
{
    /// Gets the current [`WidgetInst`] for the [`SwitchInst`].
    fn inner<'b>(&'b self, env: &Env) -> &'b Rc<Dependent<T, dyn WidgetInst<Env> + 'a>> {
        self.widget.0.with_ref(env, |cur_widget| {
            let cache: &'b _ = unsafe { &*self.cache.get() };

            // Check if the cached instance is still valid
            if let Some(cur_cache) = cache {
                if Rc::ptr_eq(cur_widget, &cur_cache.back) {
                    return cur_cache;
                }
            }

            // Update the cache
            let new_cache = Rc::new(Dependent {
                back: ManuallyDrop::new(cur_widget.clone()),
                front: ManuallyDrop::new(cur_widget.inst(
                    env,
                    SwitchSlot {
                        source: self.slot.clone(),
                    },
                )),
            });
            let new_cache: Rc<Dependent<T, dyn WidgetInst<Env> + '_>> = new_cache;
            let new_cache: Rc<Dependent<T, dyn WidgetInst<Env> + 'a>> =
                unsafe { std::mem::transmute(new_cache) };
            unsafe {
                *self.cache.get() = Some(new_cache);
            }

            // Return instance from cache
            unsafe { &*self.cache.get() }.as_ref().unwrap()
        })
    }
}

impl<
        'a,
        P: Property<Env, Value = Rc<T>>,
        T: Widget<Env> + ?Sized,
        Env: WidgetEnvironment + ?Sized,
        Slot: WidgetSlot<Env>,
    > WidgetInst<Env> for SwitchInst<'a, P, T, Env, Slot>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        self.inner(env).front.draw(env, drawer)
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env> {
        let inner = self.inner(env);
        match inner.front.cursor_event(env, pos, event) {
            CursorEventResponse::Handled => CursorEventResponse::Handled,
            CursorEventResponse::Start(req) => {
                // Wrap the handler in a `Dependent` to ensure that the `WidgetInst` is kept alive
                // while the interaction is active
                CursorEventResponse::Start(CursorInteractionRequest {
                    scope: req.scope,
                    handler: Rc::new(Dependent {
                        back: ManuallyDrop::new(inner.clone()),
                        front: ManuallyDrop::new(unsafe { std::mem::transmute(req.handler) }),
                    }),
                })
            }
            CursorEventResponse::Bubble => CursorEventResponse::Bubble,
        }
    }

    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>> {
        todo!()
    }
}

impl<'ui, Back: ?Sized, Env: WidgetEnvironment + ?Sized> CursorInteractionHandler<'ui, Env>
    for Dependent<Back, Box<dyn CursorInteractionHandler<'ui, Env> + 'ui>>
{
    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorInteractionEventResponse<'ui, Env> {
        todo!()
    }

    fn general_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: GeneralEvent,
    ) -> CursorInteractionEventResponse<'ui, Env> {
        todo!()
    }
}

/// The slot provided by a [`Switch`] widget.
#[derive(Clone)]
struct SwitchSlot<S> {
    source: Rc<S>,
}

impl<S: WidgetSlot<Env>, Env: WidgetEnvironment + ?Sized> WidgetSlot<Env> for SwitchSlot<S> {
    fn is_visible(&self, env: &Env) -> bool {
        // TODO: Consider whether the instance for the slot is still active in the switch
        self.source.is_visible(env)
    }

    fn size(&self, env: &Env) -> Size2i {
        self.source.size(env)
    }

    fn min(&self, env: &Env) -> Point2i {
        self.source.min(env)
    }

    fn bounds(&self, env: &Env) -> Box2i {
        self.source.bounds(env)
    }

    fn bubble_general_event(&self, env: &mut Env, event: GeneralEvent) {
        self.source.bubble_general_event(env, event)
    }
}
