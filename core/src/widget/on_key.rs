use crate::prelude::*;
use std::any::Any;
use std::rc::Rc;

/// Contains [`OnKey`]-related extension methods for [`Widget`].
pub trait OnKeyWidgetExt<Env: WidgetEnvironment + ?Sized>: Widget<Env> + Sized {
    /// Wraps this [`Widget`] to make it focusable and attaches the given key press callback to it.
    fn on_key<F: Fn(&mut Env, Key) -> bool>(self, on_key: F) -> OnKey<Self, F> {
        OnKey::new(self, on_key)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>> OnKeyWidgetExt<Env> for T {}

/// A wrapper over a [`Widget`] which makes it focusable and attaches a key press callback to it.
#[derive(Clone, Copy)]
pub struct OnKey<T, F> {
    inner: T,
    on_key: F,
}

impl<T, F> OnKey<T, F> {
    /// Constructs a new [`OnKey`] wrapper over the given widget with the given callback.
    pub fn new(inner: T, on_key: F) -> Self {
        OnKey { inner, on_key }
    }
}

impl<T: WidgetLike, F> WidgetLike for OnKey<T, F> {}

impl<Env: WidgetEnvironment + ?Sized, T: IntoWidget<Env>, F: Fn(&mut Env, Key) -> bool>
    IntoWidget<Env> for OnKey<T, F>
{
    fn into_widget(self, env: &Env) -> impl Widget<Env> {
        OnKey {
            inner: self.inner.into_widget(env),
            on_key: self.on_key,
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>, F: Fn(&mut Env, Key) -> bool> Widget<Env>
    for OnKey<T, F>
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.inner.sizing(env)
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(
        &'a self,
        env: &Env,
        slot: S,
    ) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        OnKeyPlaced {
            handler: &self.on_key,
            slot: slot.clone(),
            inner: self.inner.place(
                env,
                OnKeySlot {
                    handler: &self.on_key,
                    source: slot,
                },
            ),
        }
    }
}

/// A [`WidgetSlot`] provided by an [`OnKey`] widget to its inner widget.
struct OnKeySlot<'a, F, S> {
    handler: &'a F,
    source: S,
}

impl<F, S: Clone> Clone for OnKeySlot<'_, F, S> {
    fn clone(&self) -> Self {
        OnKeySlot {
            handler: self.handler,
            source: self.source.clone(),
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&mut Env, Key) -> bool, S: WidgetSlot<Env>>
    WidgetSlot<Env> for OnKeySlot<'_, F, S>
{
    fn is_visible(&self, env: &Env) -> bool {
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
        if let GeneralEvent::Key { key, is_down: true } = event {
            if !(self.handler)(env, key) {
                self.source.bubble_general_event(env, event)
            }
        } else {
            self.source.bubble_general_event(env, event)
        }
    }
}

/// A [`OnKey`] widget which has been placed in a [`WidgetSlot`].
struct OnKeyPlaced<'a, F, S, T> {
    handler: &'a F,
    slot: S,
    inner: T,
}

impl<
    Env: WidgetEnvironment + ?Sized,
    F: Fn(&mut Env, Key) -> bool,
    S: WidgetSlot<Env>,
    T: WidgetPlaced<Env>,
> WidgetPlaced<Env> for OnKeyPlaced<'_, F, S, T>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        self.inner.draw(env, drawer)
    }

    fn hover_feedback(&self, env: &Env, pos: Vector2i, feedback: &mut dyn FnMut(&dyn Any)) -> bool {
        self.inner.hover_feedback(env, pos, feedback)
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env> {
        self.inner.cursor_event(env, pos, event)
    }

    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>> {
        self.inner
            .focus(env, backward)
            .or(Some(FocusInteractionRequest {
                scope: FocusScope::KEYBOARD,
                // TODO: Remove need for boxing
                handler: Rc::new(self),
            }))
    }
}

impl<
    'ui,
    Env: WidgetEnvironment + ?Sized,
    F: Fn(&mut Env, Key) -> bool,
    S: WidgetSlot<Env>,
    T: WidgetPlaced<Env>,
> FocusInteractionHandler<'ui, Env> for OnKeyPlaced<'_, F, S, T>
{
    fn general_event(
        &self,
        env: &mut Env,
        event: GeneralEvent,
    ) -> FocusInteractionEventResponse<'ui, Env> {
        if let GeneralEvent::Key { key, is_down: true } = event {
            if !(self.handler)(env, key) {
                self.slot.bubble_general_event(env, event);
            }
        } else {
            self.slot.bubble_general_event(env, event);
        }
        FocusInteractionEventResponse::Keep
    }
}
