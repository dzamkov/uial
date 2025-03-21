use uial::prelude::*;

/// A widget style which offers all "common" widget types.
pub trait CommonStyle<Env: WidgetEnvironment + ?Sized>:
    ButtonStyle<Env> + TextButtonStyle<Env>
{
}

impl<Env: WidgetEnvironment + ?Sized, T: ButtonStyle<Env> + TextButtonStyle<Env>> CommonStyle<Env>
    for T
{
}

/// A widget style which defines a "button" widget.
pub trait ButtonStyle<Env: WidgetEnvironment + ?Sized> {
    /// Constructs a button with the given internal content.
    fn button(&self, content: impl IntoWidget<Env>) -> impl IntoButtonWidget<Env>;
}

/// A widget style which defines a "text button" widget.
pub trait TextButtonStyle<Env: WidgetEnvironment + ?Sized> {
    /// Constructs a button with the given text.
    fn text_button(&self, text: impl Property<Env, Value = str>) -> impl IntoButtonWidget<Env>;
}

/// A [`WidgetLike`] representing a clickable button.
pub trait IntoButtonWidget<Env: WidgetEnvironment + ?Sized>: IntoWidget<Env> {
    /// Sets the enabled state of this button.
    fn set_enabled(
        self,
        is_enabled: impl Property<Env, Value = bool> + Clone,
    ) -> impl IntoButtonWidget<Env>;

    /// Sets the click handler for this button.
    fn on_click(self, handler: impl Fn(&mut Env)) -> impl IntoButtonWidget<Env>;
}

/// Shortcut for [`ButtonStyle::button`].
pub fn button<
    Env: WidgetEnvironment + ?Sized + 'static,
    Style: ButtonStyle<Env> + 'static,
    Content: IntoWidget<Env>,
>(
    style: &Style,
    content: Content,
) -> impl IntoButtonWidget<Env> + use<Env, Style, Content> {
    // TODO: This is a hack to work around https://github.com/rust-lang/rust/issues/130044
    // once this is stablized, we can remove this and the `'static` bounds
    let style: &'static Style = unsafe { std::mem::transmute(style) };
    style.button(content)
}

/// Shortcut for [`TextButtonStyle::text_button`].
pub fn text_button<
    Env: WidgetEnvironment + ?Sized + 'static,
    Style: TextButtonStyle<Env> + 'static,
    PText: Property<Env, Value = str>,
>(
    style: &Style,
    text: PText,
) -> impl IntoButtonWidget<Env> + use<Env, Style, PText> {
    // TODO: This is a hack to work around https://github.com/rust-lang/rust/issues/130044
    // once this is stablized, we can remove this and the `'static` bounds
    let style: &'static Style = unsafe { std::mem::transmute(style) };
    style.text_button(text)
}
