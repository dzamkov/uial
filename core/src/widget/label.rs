use crate::drawer::{Font, FontBase, RasterDrawer, TextBuffer, TextDrawer, TextWriter};
use crate::geometry::Dir2i;
use crate::prelude::*;
use std::any::Any;

/// A [`Widget`] which displays a text label.
///
/// The text is displayed on a single line, and is not interactive. The widget is sized to exactly
/// fit the text.
pub struct Label<
    Env: WidgetEnvironment + Track + ?Sized,
    F: Property<Env>,
    T: Property<Env, Value = String>,
> where
    Env::Drawer: RasterDrawer,
    F::Value: Font<Env::Drawer>,
    <F::Value as FontBase>::Glyph: Sized,
{
    font: F,
    dir: Dir2i,
    text: T,
    #[allow(clippy::type_complexity)]
    text_buffer_cache: Cache<Env, (Size2i, TextBuffer<<F::Value as FontBase>::Glyph>)>,
}

impl<Env: WidgetEnvironment + Track + ?Sized, F: Property<Env>, T: Property<Env, Value = String>>
    Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Value: Font<Env::Drawer>,
    <F::Value as FontBase>::Glyph: Sized,
{
    /// Constructs a new [`Label`] widget.
    pub fn new(font: F, text: T) -> Self {
        Label {
            font,
            dir: Dir2i::Xp,
            text,
            text_buffer_cache: Cache::new(),
        }
    }

    /// Calls the given closure with a [`TextBuffer`] containing the arranged text of this label.
    fn with_text_buffer<R>(
        &self,
        env: &Env,
        f: impl FnOnce(Size2i, &TextBuffer<<F::Value as FontBase>::Glyph>) -> R,
    ) -> R {
        self.text_buffer_cache.with(
            env,
            |env, prev| {
                let mut res = if let Some((_, mut prev)) = prev {
                    prev.clear();
                    prev
                } else {
                    TextBuffer::new()
                };
                let mut writer = TextWriter {
                    dir: self.dir,
                    pen: vec2(0.0 as Scalar, 0.0 as Scalar),
                    line_size: 0.0,
                    target: &mut res,
                };
                self.font.with_ref(env, |font| {
                    self.text.with_ref(env, |text| {
                        writer.write(font, text);
                    })
                });
                // TODO: Support for non-LTR text
                let size = size2i(writer.pen.x.ceil() as u32, writer.line_size as u32);
                (size, res)
            },
            |(size, buffer)| f(*size, buffer),
        )
    }
}

/// Constructs a [`Widget`] which displays a text label.
pub fn label<
    Env: WidgetEnvironment + Track + ?Sized,
    F: Font<Env::Drawer>,
    T: Property<Env, Value = String>,
>(
    font: F,
    text: T,
) -> Label<Env, Const<F>, T>
where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
    Label {
        font: const_(font),
        dir: Dir2i::Xp,
        text,
        text_buffer_cache: Cache::new(),
    }
}

impl<Env: WidgetEnvironment + Track + ?Sized, F: Property<Env>, T: Property<Env, Value = String>>
    WidgetLike for Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Value: Font<Env::Drawer>,
    <F::Value as FontBase>::Glyph: Sized,
{
}

impl<Env: WidgetEnvironment + Track + ?Sized, F: Property<Env>, T: Property<Env, Value = String>>
    IntoWidget<Env> for Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Value: Font<Env::Drawer>,
    <F::Value as FontBase>::Glyph: Sized,
{
    fn into_widget(self, _: &Env) -> impl Widget<Env> {
        self
    }
}

impl<Env: WidgetEnvironment + Track + ?Sized, F: Property<Env>, T: Property<Env, Value = String>>
    Widget<Env> for Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Value: Font<Env::Drawer>,
    <F::Value as FontBase>::Glyph: Sized,
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.with_text_buffer(env, |size, _| Sizing::exact(size))
    }

    fn place<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetPlaced<Env> + 'a
    where
        Env: 'a,
    {
        LabelPlaced { widget: self, slot }
    }
}

/// A [`Label`] widget which has been placed in a [`WidgetSlot`].
struct LabelPlaced<
    'a,
    Env: WidgetEnvironment + Track + ?Sized,
    F: Property<Env>,
    T: Property<Env, Value = String>,
    Slot,
> where
    Env::Drawer: RasterDrawer,
    F::Value: Font<Env::Drawer>,
    <F::Value as FontBase>::Glyph: Sized,
{
    widget: &'a Label<Env, F, T>,
    slot: Slot,
}

impl<
    Env: WidgetEnvironment + Track + ?Sized,
    F: Property<Env>,
    T: Property<Env, Value = String>,
    Slot: WidgetSlot<Env>,
> WidgetPlaced<Env> for LabelPlaced<'_, Env, F, T, Slot>
where
    Env::Drawer: RasterDrawer,
    F::Value: Font<Env::Drawer>,
    <F::Value as FontBase>::Glyph: Sized,
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        let offset = self.slot.min(env);
        self.widget.font.with_ref(env, |font| {
            self.widget.with_text_buffer(env, |_, buffer| {
                drawer.draw_text_buffer(font, offset, buffer);
            })
        })
    }

    fn hover_feedback(&self, _: &Env, _: Vector2i, _: &mut dyn FnMut(&dyn Any)) -> bool {
        false
    }

    fn cursor_event(&self, _: &mut Env, _: Vector2i, _: CursorEvent) -> CursorEventResponse<Env> {
        CursorEventResponse::Bubble
    }

    fn focus(&self, _: &mut Env, _: bool) -> Option<FocusInteractionRequest<Env>> {
        None
    }
}
