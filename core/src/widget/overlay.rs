use super::*;

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
pub fn overlay<Below: WidgetBase, Above: WidgetBase>(
    below: Below,
    above: Above,
) -> Overlay<Below, Above> {
    Overlay { below, above }
}

impl<Below: WidgetBase, Above: WidgetBase> WidgetBase for Overlay<Below, Above> {}

impl<Env: WidgetEnvironment + ?Sized, Below: Widget<Env>, Above: Widget<Env>> Widget<Env>
    for Overlay<Below, Above>
{
    fn sizing(&self, env: &Env) -> Sizing {
        &self.below.sizing(env) & &self.above.sizing(env)
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, env: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        OverlayInst {
            below: self.below.inst(env, slot.clone()),
            above: self.above.inst(env, slot),
        }
    }
}

/// A [`WidgetInst`] for an [`Overlay`] widget.
struct OverlayInst<Below, Above> {
    below: Below,
    above: Above,
}

impl<Env: WidgetEnvironment + ?Sized, Below: WidgetInst<Env>, Above: WidgetInst<Env>>
    WidgetInst<Env> for OverlayInst<Below, Above>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        self.below.draw(env, drawer);
        self.above.draw(env, drawer);
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
            self.below.focus(env, backward).or_else(|| self.above.focus(env, backward))
        } else {
            self.above.focus(env, backward).or_else(|| self.below.focus(env, backward))
        }
    }
}
