use crate::prelude::*;
use std::any::Any;

/// Constructs a [`Widget`] by "overlaying" the given [`Widget`]s in the same layout rectangle.
#[macro_export]
macro_rules! overlay {
    [$head:expr, $($tail:expr),*] => {
        {
            let res = $head;
            $(
                let res = $crate::widget::overlay(
                    res,
                    $tail
                );
            )*
            res
        }
    }
}

/// A [`Widget`] which combines two source [`Widget`]s in the same layout rectangle, overlaying
/// one on top of the other.
pub struct Overlay<Below, Above> {
    below: Below,
    above: Above,
}

/// Constructs a [`Widget`] which overlays one widget above another in the same layout rectangle.
pub fn overlay<Below: WidgetLike, Above: WidgetLike>(
    below: Below,
    above: Above,
) -> Overlay<Below, Above> {
    Overlay { below, above }
}

impl<Below: WidgetLike, Above: WidgetLike> WidgetLike for Overlay<Below, Above> {}

impl<Env: WidgetEnvironment + ?Sized, Below: IntoWidget<Env>, Above: IntoWidget<Env>>
    IntoWidget<Env> for Overlay<Below, Above>
{
    fn into_widget(self, env: &Env) -> impl Widget<Env> {
        Overlay {
            below: self.below.into_widget(env),
            above: self.above.into_widget(env),
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, Below: Widget<Env>, Above: Widget<Env>> Widget<Env>
    for Overlay<Below, Above>
{
    fn sizing(&self, env: &Env) -> Sizing {
        &self.below.sizing(env) & &self.above.sizing(env)
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(
        &'a self,
        env: &Env,
        slot: S,
    ) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        OverlayPlaced {
            below: self.below.place(env, slot.clone()),
            above: self.above.place(env, slot),
        }
    }
}

/// An [`Overlay`] widget which has been placed in a [`WidgetSlot`].
struct OverlayPlaced<Below, Above> {
    below: Below,
    above: Above,
}

impl<Env: WidgetEnvironment + ?Sized, Below: WidgetPlaced<Env>, Above: WidgetPlaced<Env>>
    WidgetPlaced<Env> for OverlayPlaced<Below, Above>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        self.below.draw(env, drawer);
        self.above.draw(env, drawer);
    }

    fn hover_feedback(&self, env: &Env, pos: Vector2i, feedback: &mut dyn FnMut(&dyn Any)) -> bool {
        self.above.hover_feedback(env, pos, feedback)
            || self.below.hover_feedback(env, pos, feedback)
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env> {
        let above_response = self.above.cursor_event(env, pos, event);
        if let CursorEventResponse::Bubble = above_response {
            self.below.cursor_event(env, pos, event)
        } else {
            above_response
        }
    }

    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>> {
        if backward {
            self.below
                .focus(env, backward)
                .or_else(|| self.above.focus(env, backward))
        } else {
            self.above
                .focus(env, backward)
                .or_else(|| self.below.focus(env, backward))
        }
    }
}
