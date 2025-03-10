use crate::prelude::*;
use std::any::Any;
use std::rc::Rc;

/// Contains [`OnClick`]-related extension methods for [`Widget`]s.
pub trait OnClickWidgetExt<Env: WidgetEnvironment + ?Sized>: Widget<Env> + Sized {
    fn on_click<F: Fn(&mut Env)>(self, on_click: F) -> OnClick<Self, F> {
        OnClick::new(self, on_click)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>> OnClickWidgetExt<Env> for T {}

/// A wrapper over a [`Widget`] which makes it clickable and attaches a click callback to it.
#[derive(Clone, Copy)]
pub struct OnClick<T, F> {
    inner: T,
    on_click: F,
}

impl<T, F> OnClick<T, F> {
    pub fn new(inner: T, on_click: F) -> Self {
        OnClick { inner, on_click }
    }
}

impl<T: WidgetLike, F> WidgetLike for OnClick<T, F> {}

impl<Env: WidgetEnvironment + ?Sized, T: IntoWidget<Env>, F: Fn(&mut Env)> IntoWidget<Env>
    for OnClick<T, F>
{
    fn into_widget(self, env: &Env) -> impl Widget<Env> {
        OnClick {
            inner: self.inner.into_widget(env),
            on_click: self.on_click,
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>, F: Fn(&mut Env)> Widget<Env>
    for OnClick<T, F>
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
        OnClickPlaced {
            handler: &self.on_click,
            slot: slot.clone(),
            inner: self.inner.place(env, slot),
        }
    }
}

/// An [`OnClick`] widget which has been placed in a [`WidgetSlot`].
struct OnClickPlaced<'a, F, S, T> {
    handler: &'a F,
    slot: S,
    inner: T,
}

impl<Env: WidgetEnvironment + ?Sized, F: Fn(&mut Env), S: WidgetSlot<Env>, T: WidgetPlaced<Env>>
    WidgetPlaced<Env> for OnClickPlaced<'_, F, S, T>
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
        let res = self.inner.cursor_event(env, pos, event);
        if let CursorEvent::MouseButton {
            button: winit::event::MouseButton::Left,
            is_down: true,
        } = event
        {
            match res {
                CursorEventResponse::Handled => CursorEventResponse::Handled,
                CursorEventResponse::Start(_) => todo!(),
                CursorEventResponse::Bubble => {
                    if self.slot.bounds(env).contains(pos) {
                        CursorEventResponse::Start(CursorInteractionRequest {
                            scope: FocusScope::NONE,
                            // TODO: Remove need for boxing
                            handler: Rc::new(self),
                        })
                    } else {
                        CursorEventResponse::Bubble
                    }
                }
            }
        } else {
            res
        }
    }

    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>> {
        self.inner.focus(env, backward)
    }
}

impl<
    'ui,
    Env: WidgetEnvironment + ?Sized,
    F: Fn(&mut Env),
    S: WidgetSlot<Env>,
    T: WidgetPlaced<Env>,
> CursorInteractionHandler<'ui, Env> for OnClickPlaced<'_, F, S, T>
{
    fn is_locked(&self, _: &Env) -> bool {
        true
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorInteractionEventResponse<'ui, Env> {
        if let CursorEvent::MouseButton {
            button: winit::event::MouseButton::Left,
            is_down: false,
        } = event
        {
            if self.slot.bounds(env).contains(pos) {
                (self.handler)(env);
            }
            CursorInteractionEventResponse::End
        } else {
            CursorInteractionEventResponse::Keep
        }
    }

    fn general_event(
        &self,
        env: &mut Env,
        _: Vector2i,
        event: GeneralEvent,
    ) -> CursorInteractionEventResponse<'ui, Env> {
        self.slot.bubble_general_event(env, event);
        CursorInteractionEventResponse::Keep
    }

    fn feedback(&self, _: &Env, _: &mut dyn FnMut(&dyn Any)) {
        // No feedback
    }
}
