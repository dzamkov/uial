use crate::imp::{RunEnvironment, RunFont, RunImageHandle, StretchableImage};
use std::rc::Rc;
use uial::drawer::{HasImageManager, ImageHandle};
use uial::prelude::*;

/// Encapsulates the styling information for a [`Button`].
pub struct ButtonStyle<I: ImageHandle> {
    /// The appearance of the button in its normal state.
    pub normal: StretchableImage<I>,

    /// The appearance of the button when it is hovered over.
    pub hover: StretchableImage<I>,

    /// The appearance of the button when it is pressed.
    pub pressed: StretchableImage<I>,

    /// The appearance of the button when it is disabled.
    pub disabled: StretchableImage<I>,

    /// The padding applied to the content inside of the button.
    pub padding: Padding2i,
}

/// Encapsulates the styling information for a [`Button`] which displays text.
pub struct TextButtonStyle<I: ImageHandle, F: Clone> {
    /// The style of the underlying button.
    pub base: Rc<ButtonStyle<I>>,

    /// The font used to render the text of the button when it is enabled.
    pub enabled_font: F,

    /// The font used to render the text of the button when it is disabled.
    pub disabled_font: F,
}

impl<Env: RunEnvironment + ?Sized> crate::ButtonStyle<Env>
    for Rc<ButtonStyle<RunImageHandle<Env>>>
{
    fn button(&self, content: impl IntoWidget<Env>) -> impl crate::IntoButtonWidget<Env> {
        ButtonBuilder {
            def: (self.clone(), content),
            is_enabled: const_(true),
            on_click: on_click_default,
        }
    }
}

impl<Env: RunEnvironment + ?Sized> crate::TextButtonStyle<Env>
    for TextButtonStyle<RunImageHandle<Env>, RunFont<Env>>
{
    fn text_button(
        &self,
        text: impl Property<Env, Value = String>,
    ) -> impl crate::IntoButtonWidget<Env> {
        ButtonBuilder {
            def: (self, text),
            is_enabled: const_(true),
            on_click: on_click_default,
        }
    }
}

/// Default click handler for a [`Button`].
fn on_click_default<Env: WidgetEnvironment + ?Sized>(_: &mut Env) {}

/// A builder for constructing [`Button`]s.
struct ButtonBuilder<Def, Enabled, OnClick> {
    def: Def,
    is_enabled: Enabled,
    on_click: OnClick,
}

impl<Def, Enabled, OnClick> WidgetLike for ButtonBuilder<Def, Enabled, OnClick> {}

impl<
    Env: RunEnvironment + ?Sized,
    Def: ButtonDefinition<Env>,
    Enabled: Property<Env, Value = bool> + Clone,
    OnClick: Fn(&mut Env),
> crate::IntoButtonWidget<Env> for ButtonBuilder<Def, Enabled, OnClick>
{
    fn set_enabled(
        self,
        is_enabled: impl Property<Env, Value = bool> + Clone,
    ) -> impl crate::IntoButtonWidget<Env> {
        ButtonBuilder {
            def: self.def,
            is_enabled,
            on_click: self.on_click,
        }
    }

    fn on_click(self, handler: impl Fn(&mut Env)) -> impl crate::IntoButtonWidget<Env> {
        ButtonBuilder {
            def: self.def,
            is_enabled: self.is_enabled,
            on_click: handler,
        }
    }
}

impl<
    Env: WidgetEnvironment + ?Sized,
    Def: ButtonDefinition<Env>,
    Enabled: Property<Env, Value = bool> + Clone,
    OnClick: Fn(&mut Env),
> IntoWidget<Env> for ButtonBuilder<Def, Enabled, OnClick>
{
    fn into_widget(self, env: &Env) -> impl Widget<Env> {
        Def::into_widget(self, env)
    }
}

/// Encapsulates the content and style of a [`Button`].
trait ButtonDefinition<Env: WidgetEnvironment + ?Sized>: Sized {
    /// Constructs a [`Widget`] from a builder using this definition.
    fn into_widget<Enabled: Property<Env, Value = bool> + Clone, OnClick: Fn(&mut Env)>(
        builder: ButtonBuilder<Self, Enabled, OnClick>,
        env: &Env,
    ) -> impl Widget<Env>;
}

impl<Env: RunEnvironment + ?Sized, Content: IntoWidget<Env>> ButtonDefinition<Env>
    for (Rc<ButtonStyle<RunImageHandle<Env>>>, Content)
{
    fn into_widget<Enabled: Property<Env, Value = bool> + Clone, OnClick: Fn(&mut Env)>(
        builder: ButtonBuilder<Self, Enabled, OnClick>,
        env: &Env,
    ) -> impl Widget<Env> {
        let (style, content) = builder.def;
        let padding = style.padding;
        overlay![
            Button::new(style, builder.is_enabled, builder.on_click),
            content.into_widget(env).center().with_padding(padding)
        ]
    }
}
impl<Env: RunEnvironment + ?Sized, Text: Property<Env, Value = String>> ButtonDefinition<Env>
    for (&'_ TextButtonStyle<RunImageHandle<Env>, RunFont<Env>>, Text)
{
    fn into_widget<Enabled: Property<Env, Value = bool> + Clone, OnClick: Fn(&mut Env)>(
        builder: ButtonBuilder<Self, Enabled, OnClick>,
        _: &Env,
    ) -> impl Widget<Env> {
        let (style, text) = builder.def;
        let padding = style.base.padding;
        overlay![
            Button::new(
                style.base.clone(),
                builder.is_enabled.clone(),
                builder.on_click
            ),
            widget::Label::new(
                SwitchProperty {
                    switch: builder.is_enabled,
                    on_false: const_(style.disabled_font.clone()),
                    on_true: const_(style.enabled_font.clone()),
                },
                text
            )
            .center()
            .with_padding(padding)
        ]
    }
}

/// A property which defers to another property based on a boolean switch.
struct SwitchProperty<E, T> {
    switch: E,
    on_false: T,
    on_true: T,
}

impl<E, T: PropertyBase> PropertyBase for SwitchProperty<E, T> {
    type Value = T::Value;
}

impl<Env: ?Sized, E: Property<Env, Value = bool>, T: Property<Env>> Property<Env>
    for SwitchProperty<E, T>
{
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&T::Value) -> R) -> R {
        if self.switch.get(env) {
            self.on_true.with_ref(env, inner)
        } else {
            self.on_false.with_ref(env, inner)
        }
    }
}

/// A [`Widget`] which displays a clickable button (just the base, without any internal content).
pub struct Button<Env: WidgetEnvironment + HasImageManager + ?Sized, Enabled, OnClick> {
    id: WidgetId,
    style: Rc<ButtonStyle<RunImageHandle<Env>>>,
    is_enabled: Enabled,
    on_click: OnClick,
}

/// Identifies the visual state of a [`Button`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ButtonState {
    Normal,
    Hover,
    Pressed,
    Disabled,
}

impl<Env: WidgetEnvironment + HasImageManager + ?Sized, Enabled, OnClick>
    Button<Env, Enabled, OnClick>
{
    /// Creates a new [`Button`] with the given properties.
    pub fn new(
        style: Rc<ButtonStyle<RunImageHandle<Env>>>,
        is_enabled: Enabled,
        on_click: OnClick,
    ) -> Self {
        Self {
            id: WidgetId::new(),
            style,
            is_enabled,
            on_click,
        }
    }

    /// Returns a [`Property`] which represents the [`ButtonState`] of this [`Button`].
    pub fn state(&self) -> ButtonStateProperty<Enabled>
    where
        Enabled: Clone,
    {
        ButtonStateProperty {
            is_enabled: self.is_enabled.clone(),
            widget: self.id,
        }
    }
}

/// A [`Property`] which represents the [`ButtonState`] of an [`Button`].
pub struct ButtonStateProperty<Enabled> {
    is_enabled: Enabled,
    widget: WidgetId,
}

impl<Enabled> PropertyBase for ButtonStateProperty<Enabled> {
    type Value = ButtonState;
}

impl<Env: WidgetEnvironment + ?Sized, Enabled: Property<Env, Value = bool>> Property<Env>
    for ButtonStateProperty<Enabled>
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

impl<Env: RunEnvironment + ?Sized, Enabled, OnClick> WidgetLike for Button<Env, Enabled, OnClick> {}

impl<
    Env: RunEnvironment + ?Sized,
    Enabled: Property<Env, Value = bool> + Clone,
    OnClick: Fn(&mut Env),
> IntoWidget<Env> for Button<Env, Enabled, OnClick>
{
    fn into_widget(self, _: &Env) -> impl Widget<Env> {
        self
    }
}

impl<
    Env: RunEnvironment + ?Sized,
    Enabled: Property<Env, Value = bool> + Clone,
    OnClick: Fn(&mut Env),
> Widget<Env> for Button<Env, Enabled, OnClick>
{
    fn sizing(&self, _: &Env) -> Sizing {
        let mut min = size2i(1, 1);
        for s in &[
            &self.style.normal,
            &self.style.hover,
            &self.style.pressed,
            &self.style.disabled,
        ] {
            let s_min = s.min_size();
            min.x_minus_1 = min.x_minus_1.max(s_min.x_minus_1);
            min.y_minus_1 = min.y_minus_1.max(s_min.y_minus_1);
        }
        Sizing::minimum(min)
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        ButtonPlaced { widget: self, slot }
    }
}

/// A [`Button`] widget which has been placed in a [`WidgetSlot`].
struct ButtonPlaced<'a, Env: RunEnvironment + ?Sized, Enabled, OnClick, Slot> {
    widget: &'a Button<Env, Enabled, OnClick>,
    slot: Slot,
}

impl<
    Env: RunEnvironment + ?Sized,
    Enabled: Property<Env, Value = bool> + Clone,
    OnClick: Fn(&mut Env),
    Slot: WidgetSlot<Env>,
> WidgetPlaced<Env> for ButtonPlaced<'_, Env, Enabled, OnClick, Slot>
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
            Ortho2i::translate(bounds.min()),
        )
    }

    fn hover_feedback(
        &self,
        env: &Env,
        pos: Vector2i,
        f: &mut dyn FnMut(&dyn std::any::Any),
    ) -> bool {
        if self.slot.bounds(env).contains(pos) {
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
                if bounds.contains(pos) {
                    return CursorEventResponse::Start(CursorInteractionRequest {
                        scope: FocusScope::NONE,
                        handler: Rc::new(self),
                    });
                }
            }
        }
        CursorEventResponse::Bubble
    }

    fn focus(&self, _: &mut Env, _: bool) -> Option<FocusInteractionRequest<Env>> {
        // TODO: Allow focusing the button
        None
    }
}

impl<'a, Env: RunEnvironment + ?Sized, Enabled, OnClick: Fn(&mut Env), Slot: WidgetSlot<Env>>
    CursorInteractionHandler<'a, Env> for ButtonPlaced<'a, Env, Enabled, OnClick, Slot>
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
            if bounds.contains(pos) {
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

/// A feedback item that indicates that an [`Button`] is currently being hovered over.
struct HoverFeedback(WidgetId);

/// A feedback item that indicates that an [`Button`] is currently being pressed.
struct PressedFeedback(WidgetId);
