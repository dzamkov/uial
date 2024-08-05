use crate::{ButtonStyle, RunFont, RunImageHandle};
use std::rc::Rc;
use uial::drawer::{HasImageManager, ImageDrawer, ImageHandle, ImageManager};
use uial::*;

/// A helper for constructing a text button widget.
pub struct TextButtonBuilder<'a, Env: WidgetEnvironment + HasImageManager + Track + ?Sized> {
    /// The style of the button.
    pub style: Rc<TextButtonStyle<RunImageHandle<Env>, RunFont<Env>>>,

    /// Determines whether the button is enabled.
    pub is_enabled: ConstOrRcDynProperty<'a, Env, bool>,

    /// Provides the text to be displayed on the button.
    pub text: ConstOrRcDynProperty<'a, Env, String>,

    /// The handler to be called when the button is clicked.
    pub on_click: Box<dyn Fn(&mut Env) + 'a>,
}

/// Encapsulates the styling information for a text button.
pub struct TextButtonStyle<I: ImageHandle, F: Clone> {
    /// The style of the underlying button.
    pub base: Rc<ButtonStyle<I>>,

    /// The padding applied to the text inside the button.
    pub padding: Padding2i,

    /// The font used to render the text of the button when it is enabled.
    pub enabled_font: F,

    /// The font used to render the text of the button when it is disabled.
    pub disabled_font: F,
}

impl<'a, Env: WidgetEnvironment + HasImageManager + Track + ?Sized> TextButtonBuilder<'a, Env> {
    /// Constructs the text button widget described by this builder.
    pub fn build(self) -> impl Widget<Env> + 'a
    where
        Env: 'a,
        Env::Drawer: ImageDrawer<<Env::ImageManager as ImageManager>::Source>,
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
            .center()
            .with_padding(self.style.padding)
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
pub fn text_button<'a, Env: WidgetEnvironment + HasImageManager + Track + ?Sized>(
    style: Rc<TextButtonStyle<RunImageHandle<Env>, RunFont<Env>>>,
    text: String,
    on_click: impl Fn(&mut Env) + 'a,
) -> impl Widget<Env> + 'a
where
    Env: 'a,
    Env::Drawer: ImageDrawer<<Env::ImageManager as ImageManager>::Source>,
{
    TextButtonBuilder {
        style,
        is_enabled: const_(true).into(),
        text: const_(text).into(),
        on_click: Box::new(on_click),
    }
    .build()
}
