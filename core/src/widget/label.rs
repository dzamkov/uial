use super::*;
use crate::drawer::{Font, RasterDrawer, TextBuffer, TextDrawer, TextWriter};
use uial_geometry::Dir2i;

/// A [`Widget`] which displays a text label.
///
/// The text is displayed on a single line, and is not interactive. The widget is sized to exactly
/// fit the text.
pub struct Label<
    Env: WidgetEnvironment + Track + ?Sized,
    F: Font<Env::Drawer>,
    T: Property<Env, Value = String>,
> where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
    font: F,
    dir: Dir2i,
    text: T,
    text_buffer_cache: Cache<Env, (Size2i, TextBuffer<F::Glyph>)>,
}

impl<
        Env: WidgetEnvironment + Track + ?Sized,
        F: Font<Env::Drawer>,
        T: Property<Env, Value = String>,
    > Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
    /// Calls the given closure with a [`TextBuffer`] containing the arranged text of this label.
    fn with_text_buffer<R>(
        &self,
        env: &Env,
        f: impl FnOnce(Size2i, &TextBuffer<F::Glyph>) -> R,
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
                self.text.with_ref(env, |text| {
                    writer.write(&self.font, text);
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
) -> Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
    Label {
        font,
        dir: Dir2i::PosX,
        text,
        text_buffer_cache: Cache::new(),
    }
}

impl<
        Env: WidgetEnvironment + Track + ?Sized,
        F: Font<Env::Drawer>,
        T: Property<Env, Value = String>,
    > WidgetBase for Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
}

impl<
        Env: WidgetEnvironment + Track + ?Sized,
        F: Font<Env::Drawer>,
        T: Property<Env, Value = String>,
    > Widget<Env> for Label<Env, F, T>
where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.with_text_buffer(env, |size, _| Sizing::exact(size))
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        LabelInst { widget: self, slot }
    }
}

/// An instance of a [`Label`] widget.
struct LabelInst<
    'a,
    Env: WidgetEnvironment + Track + ?Sized,
    F: Font<Env::Drawer>,
    T: Property<Env, Value = String>,
    Slot,
> where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
    widget: &'a Label<Env, F, T>,
    slot: Slot,
}

impl<
        'a,
        Env: WidgetEnvironment + Track + ?Sized,
        F: Font<Env::Drawer>,
        T: Property<Env, Value = String>,
        Slot: WidgetSlot<Env>,
    > WidgetInst<Env> for LabelInst<'a, Env, F, T, Slot>
where
    Env::Drawer: RasterDrawer,
    F::Glyph: Sized,
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        let offset = self.slot.min(env);
        self.widget.with_text_buffer(env, |_, buffer| {
            drawer.draw_text_buffer(&self.widget.font, offset, buffer);
        });
    }

    fn cursor_event(&self, _: &mut Env, _: Vector2i, _: CursorEvent) -> CursorEventResponse<Env> {
        CursorEventResponse::Bubble
    }

    fn focus(&self, _: &mut Env, _: bool) -> Option<FocusInteractionRequest<Env>> {
        None
    }
}
