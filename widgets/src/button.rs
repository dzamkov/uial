use crate::{RunImageHandle, StretchableImage};
use std::rc::Rc;
use uial::drawer::{HasImageManager, ImageDrawer, ImageHandle, ImageManager};
use uial::*;

/// A [`Widget`] which displays a clickable button (just the base, without any internal content).
pub struct Button<'a, Env: WidgetEnvironment + HasImageManager + ?Sized> {
    id: WidgetId,
    style: Rc<ButtonStyle<RunImageHandle<Env>>>,
    is_enabled: ConstOrRcDynProperty<'a, Env, bool>,
    on_click: Box<dyn Fn(&mut Env) + 'a>,
}

/// Encapsulates the styling information for a [`Button`].
pub struct ButtonStyle<H: ImageHandle> {
    /// The appearance of the button in its normal state.
    pub normal: StretchableImage<H>,

    /// The appearance of the button when it is hovered over.
    pub hover: StretchableImage<H>,

    /// The appearance of the button when it is pressed.
    pub pressed: StretchableImage<H>,

    /// The appearance of the button when it is disabled.
    pub disabled: StretchableImage<H>,
}

/// Identifies the visual state of a [`Button`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ButtonState {
    Normal,
    Hover,
    Pressed,
    Disabled,
}

impl<'a, Env: WidgetEnvironment + HasImageManager + ?Sized> Button<'a, Env> {
    /// Creates a new [`Button`] with the given properties.
    pub fn new(
        style: Rc<ButtonStyle<RunImageHandle<Env>>>,
        is_enabled: ConstOrRcDynProperty<'a, Env, bool>,
        on_click: Box<dyn Fn(&mut Env) + 'a>,
    ) -> Self {
        Self {
            id: WidgetId::new(),
            style,
            is_enabled,
            on_click,
        }
    }

    /// Returns a [`Property`] which represents the [`ButtonState`] of this [`Button`].
    pub fn state(&self) -> ButtonStateProperty<'a, Env> {
        ButtonStateProperty {
            is_enabled: self.is_enabled.clone(),
            widget: self.id,
        }
    }
}

/// A [`Property`] which represents the [`ButtonState`] of a [`Button`].
pub struct ButtonStateProperty<'a, Env: ?Sized> {
    is_enabled: ConstOrRcDynProperty<'a, Env, bool>,
    widget: WidgetId,
}

impl<Env: ?Sized> PropertyBase for ButtonStateProperty<'_, Env> {
    type Value = ButtonState;
}

impl<Env: WidgetEnvironment + ?Sized> Property<Env> for ButtonStateProperty<'_, Env> {
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&ButtonState) -> R) -> R {
        inner(&self.get(env))
    }

    fn get(&self, env: &Env) -> ButtonState {
        if !self.is_enabled.get(env) {
            return ButtonState::Disabled;
        }
        let mut state = ButtonState::Normal;
        env.interaction_feedback(&mut |item| {
            if let Some(HoverFeedback(id)) = item.downcast_ref() {
                if *id == self.widget {
                    state = ButtonState::Hover;
                }
            }
            if let Some(PressedFeedback(id)) = item.downcast_ref() {
                if *id == self.widget {
                    state = ButtonState::Pressed;
                }
            }
        });
        state
    }
}

/// Shortcut for creating a [`Button`] that is always enabled.
pub fn button<'a, Env: WidgetEnvironment + HasImageManager + ?Sized>(
    style: Rc<ButtonStyle<RunImageHandle<Env>>>,
    on_click: impl Fn(&mut Env) + 'a,
) -> Button<'a, Env> {
    Button::new(style, const_(true).into(), Box::new(on_click))
}

impl<Env: WidgetEnvironment + HasImageManager + ?Sized> WidgetBase for Button<'_, Env> {}

impl<Env: WidgetEnvironment + HasImageManager + ?Sized> Widget<Env> for Button<'_, Env>
where
    Env::Drawer: ImageDrawer<<Env::ImageManager as ImageManager>::Source>,
{
    fn sizing(&self, _: &Env) -> Sizing {
        let mut min = size2i(0, 0);
        for s in &[
            &self.style.normal,
            &self.style.hover,
            &self.style.pressed,
            &self.style.disabled,
        ] {
            let s_min = s.min_size();
            min.x = min.x.max(s_min.x);
            min.y = min.y.max(s_min.y);
        }
        Sizing::range(min, size2i(u32::MAX, u32::MAX))
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        ButtonInst { widget: self, slot }
    }
}

/// An instance of a [`Button`] widget.
struct ButtonInst<'a, Env: WidgetEnvironment + HasImageManager + ?Sized, Slot> {
    widget: &'a Button<'a, Env>,
    slot: Slot,
}

impl<'a, Env: WidgetEnvironment + HasImageManager + ?Sized, Slot: WidgetSlot<Env>> WidgetInst<Env>
    for ButtonInst<'a, Env, Slot>
where
    Env::Drawer: ImageDrawer<<Env::ImageManager as ImageManager>::Source>,
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        let bounds = self.slot.bounds(env);
        let image = match self.widget.state().get(env) {
            ButtonState::Normal => &self.widget.style.normal,
            ButtonState::Hover => &self.widget.style.hover,
            ButtonState::Pressed => &self.widget.style.pressed,
            ButtonState::Disabled => &self.widget.style.disabled,
        };
        image.draw_to(
            bounds.size(),
            drawer,
            srgba(1.0, 1.0, 1.0, 1.0),
            Ortho2i::translate(bounds.min),
        )
    }

    fn hover_feedback(
        &self,
        env: &Env,
        pos: Vector2i,
        f: &mut dyn FnMut(&dyn std::any::Any),
    ) -> bool {
        if self.slot.bounds(env).contains_exclusive(pos) {
            f(&HoverFeedback(self.widget.id));
            true
        } else {
            false
        }
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env> {
        if let CursorEvent::MouseButton {
            button: MouseButton::Left,
            is_down: true,
        } = event
        {
            if self.widget.is_enabled.get(env) {
                let bounds = self.slot.bounds(env);
                if bounds.contains_exclusive(pos) {
                    return CursorEventResponse::Start(CursorInteractionRequest {
                        scope: FocusScope::NONE,
                        handler: Rc::new(self),
                    });
                }
            }
        }
        CursorEventResponse::Bubble
    }

    fn focus(&self, env: &mut Env, backward: bool) -> Option<FocusInteractionRequest<Env>> {
        // TODO: Allow focusing the button
        None
    }
}

impl<'a, Env: WidgetEnvironment + HasImageManager + ?Sized, Slot: WidgetSlot<Env>>
    CursorInteractionHandler<'a, Env> for ButtonInst<'a, Env, Slot>
{
    fn is_locked(&self, _: &Env) -> bool {
        false
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorInteractionEventResponse<'a, Env> {
        if let CursorEvent::MouseButton {
            button: MouseButton::Left,
            is_down: false,
        } = event
        {
            let bounds = self.slot.bounds(env);
            if bounds.contains_exclusive(pos) {
                (self.widget.on_click)(env);
            }
            CursorInteractionEventResponse::End
        } else {
            CursorInteractionEventResponse::Keep
        }
    }

    fn general_event(
        &self,
        _: &mut Env,
        _: Vector2i,
        _: GeneralEvent,
    ) -> CursorInteractionEventResponse<'a, Env> {
        CursorInteractionEventResponse::Keep
    }

    fn feedback(&self, _: &Env, f: &mut dyn FnMut(&dyn std::any::Any)) {
        f(&PressedFeedback(self.widget.id));
    }
}

/// A feedback item that indicates that a [`Button`] is currently being hovered over.
struct HoverFeedback(WidgetId);

/// A feedback item that indicates that a [`Button`] is currently being pressed.
struct PressedFeedback(WidgetId);
