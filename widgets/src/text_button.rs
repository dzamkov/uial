use crate::ButtonStyle;
use std::rc::Rc;
use uial::drawer::{Font, Image, ImageDrawer};
use uial::*;

/// A helper for constructing a text button widget.
pub struct TextButtonBuilder<
    I: Image,
    Font: Clone,
    E: PropertyBase<Value = bool>,
    T: PropertyBase<Value = String>,
    F,
> {
    /// The style of the button.
    pub style: Rc<TextButtonStyle<I, Font>>,

    /// Determines whether the button is enabled.
    pub is_enabled: E,

    /// Provides the text to be displayed on the button.
    pub text: T,

    /// The handler to be called when the button is clicked.
    pub on_click: F,
}

/// Encapsulates the styling information for a text button.
pub struct TextButtonStyle<I: Image, F: Clone> {
    /// The style of the underlying button.
    pub base: Rc<ButtonStyle<I>>,

    /// The font used to render the text of the button when it is enabled.
    pub enabled_font: F,

    /// The font used to render the text of the button when it is disabled.
    pub disabled_font: F,
}

impl<I: Image, Font: Clone, E: PropertyBase<Value = bool>, T: PropertyBase<Value = String>, F>
    TextButtonBuilder<I, Font, E, T, F>
{
    /// Constructs the text button widget described by this builder.
    pub fn build<Env: WidgetEnvironment + Track + ?Sized>(self) -> impl Widget<Env>
    where
        Env::Drawer: ImageDrawer<I::Store>,
        Font: uial::drawer::Font<Env::Drawer>,
        Font::Glyph: Sized,
        E: Property<Env, Value = bool> + Clone,
        T: Property<Env, Value = String>,
        F: Fn(&mut Env),
    {
        overlay![
            crate::Button::new(
                self.style.base.clone(),
                self.is_enabled.clone(),
                self.on_click
            ),
            widget::Label::new(
                SwitchProperty {
                    switch: self.is_enabled,
                    on_false: const_(self.style.disabled_font.clone()),
                    on_true: const_(self.style.enabled_font.clone()),
                },
                self.text
            )
            .center_h()
            .center_v()
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

/// Shortcut for creating a text button that is always enabled.
pub fn text_button<
    Env: WidgetEnvironment + Track + ?Sized,
    I: Image,
    F: Font<Env::Drawer> + Clone,
>(
    style: Rc<TextButtonStyle<I, F>>,
    text: String,
    on_click: impl Fn(&mut Env),
) -> impl Widget<Env>
where
    Env::Drawer: ImageDrawer<I::Store>,
    F::Glyph: Sized,
{
    TextButtonBuilder {
        style,
        is_enabled: const_(true),
        text: const_(text),
        on_click,
    }
    .build()
}
