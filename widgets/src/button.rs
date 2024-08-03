use crate::Stretchable;
use drawer::ImageDrawer;
use std::rc::Rc;
use uial::drawer::Image;
use uial::*;

/// A [`Widget`] which displays a clickable button (just the base, without any internal content).
pub struct Button<I: Image, E: PropertyBase<Value = bool>, F> {
    id: WidgetId,
    style: Rc<ButtonStyle<I>>,
    is_enabled: E,
    on_click: F,
}

/// Encapsulates the styling information for a [`Button`].
pub struct ButtonStyle<I: Image> {
    /// The appearance of the button in its normal state.
    pub normal: Stretchable<I>,

    /// The appearance of the button when it is hovered over.
    pub hover: Stretchable<I>,

    /// The appearance of the button when it is pressed.
    pub pressed: Stretchable<I>,

    /// The appearance of the button when it is disabled.
    pub disabled: Stretchable<I>,
}

/// Identifies the visual state of a [`Button`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ButtonState {
    Normal,
    Hover,
    Pressed,
    Disabled,
}

impl<I: Image, E: PropertyBase<Value = bool> + Clone, F> Button<I, E, F> {
    /// Creates a new [`Button`] with the given properties.
    pub fn new(style: Rc<ButtonStyle<I>>, is_enabled: E, on_click: F) -> Self {
        Self {
            id: WidgetId::new(),
            style,
            is_enabled,
            on_click,
        }
    }

    /// Returns a [`Property`] which represents the [`ButtonState`] of this [`Button`].
    pub fn state(&self) -> ButtonStateProperty<E> {
        ButtonStateProperty {
            is_enabled: self.is_enabled.clone(),
            widget: self.id,
        }
    }
}

/// A [`Property`] which represents the [`ButtonState`] of a [`Button`].
#[derive(Clone, Copy)]
pub struct ButtonStateProperty<E: PropertyBase<Value = bool>> {
    is_enabled: E,
    widget: WidgetId,
}

impl<E: PropertyBase<Value = bool>> PropertyBase for ButtonStateProperty<E> {
    type Value = ButtonState;
}

impl<Env: WidgetEnvironment + ?Sized, E: Property<Env, Value = bool>> Property<Env>
    for ButtonStateProperty<E>
{
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&ButtonState) -> R) -> R {
        inner(&self.get(env))
    }

    fn get(&self, env: &Env) -> ButtonState {
        if !self.is_enabled.get(env) {
            return ButtonState::Disabled;
        }
        let mut state = ButtonState::Normal;
        env.interaction_feedback(&mut |item| {
            if let Some(HoverFeedback { widget, .. }) = item.downcast_ref() {
                if *widget == self.widget {
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
pub fn button<Env: WidgetEnvironment + ?Sized, I: Image>(
    style: Rc<ButtonStyle<I>>,
    on_click: impl Fn(&mut Env),
) -> Button<I, Const<bool>, impl Fn(&mut Env)> {
    Button::new(style, const_(true), on_click)
}

impl<I: Image, E: PropertyBase<Value = bool>, F> WidgetBase for Button<I, E, F> {}

impl<
        Env: WidgetEnvironment + ?Sized,
        I: Image,
        E: Property<Env, Value = bool> + Clone,
        F: Fn(&mut Env),
    > Widget<Env> for Button<I, E, F>
where
    Env::Drawer: ImageDrawer<I::Store>,
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
struct ButtonInst<'a, I: Image, E: PropertyBase<Value = bool>, F, Slot> {
    widget: &'a Button<I, E, F>,
    slot: Slot,
}

impl<
        'a,
        Env: WidgetEnvironment + ?Sized,
        I: Image,
        E: Property<Env, Value = bool> + Clone,
        F: Fn(&mut Env),
        Slot: WidgetSlot<Env>,
    > WidgetInst<Env> for ButtonInst<'a, I, E, F, Slot>
where
    Env::Drawer: ImageDrawer<I::Store>,
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

    fn identify(&self, env: &Env, pos: Vector2i) -> Option<WidgetId> {
        if self.slot.bounds(env).contains_exclusive(pos) {
            Some(self.widget.id)
        } else {
            None
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

impl<
        'a,
        Env: WidgetEnvironment + ?Sized,
        I: Image,
        E: Property<Env, Value = bool>,
        F: Fn(&mut Env),
        Slot: WidgetSlot<Env>,
    > CursorInteractionHandler<'a, Env> for ButtonInst<'a, I, E, F, Slot>
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

/// A feedback item that indicates that a [`Button`] is currently being pressed.
struct PressedFeedback(WidgetId);
