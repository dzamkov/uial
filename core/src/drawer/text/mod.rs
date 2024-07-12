mod image_tt;
use super::*;
use uial_geometry::Dir2i;
pub use image_tt::*;
use std::iter::Peekable;
use std::marker::PhantomData;

/// Describes a method of converting arbitrary strings of text into a form that can be drawn
/// to a [`RasterDrawer`]. This fully  captures all information about the appearance/style of the
/// text, including size, color and weight.
pub trait Font {
    /// The type of "glyph"s produced by this [`Font`].
    type Glyph<'font>: ?Sized
    where
        Self: 'font;

    /// Converts a stream of characters into glyphs using a [`TextWriter`].
    fn write_to<'font>(
        &'font self,
        writer: &mut TextWriter<impl GlyphPlacer<Glyph = Self::Glyph<'font>>>,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    );
}

/// Describes a primitive drawable unit for a [`Font`] which can be drawn to a certain type of
/// [`RasterDrawer`]. The glyph includes shape, size, color and weight information, but does not
/// include positioning information.
pub trait Glyph<Drawer: RasterDrawer + ?Sized> {
    /// Draws this [`Glyph`] to the given drawer at the given offset.
    fn draw_to(&self, drawer: &mut Drawer, offset: Vector2i);
}

/// An interface for placing glyphs.
pub trait GlyphPlacer {
    /// The type of [`Glyph`] accepted by this [`GlyphPlacer`].
    type Glyph: ?Sized;

    /// Gets a mutable reference to the last placed glyph, or [`None`] if this is unavailable
    /// for any reason. The glyph may be modified, which can be used, for example, to implement
    /// ligatures.
    fn peek_last(&mut self) -> Option<&mut (Vector2i, Self::Glyph)>;

    /// Attempts to remove the last placed glyph, returning `false` if this is not possible.
    fn pop_last(&mut self) -> bool;

    /// Places a glyph at the given offset.
    fn place(&mut self, offset: Vector2i, glyph: Self::Glyph)
    where
        Self::Glyph: Sized;
}

impl<T: Font + ?Sized> Font for &'_ T {
    type Glyph<'font> = T::Glyph<'font>
    where
        Self: 'font;

    fn write_to<'font>(
        &'font self,
        writer: &mut TextWriter<impl GlyphPlacer<Glyph = Self::Glyph<'font>>>,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) {
        (**self).write_to(writer, chars);
    }
}

impl<T: Glyph<Drawer> + ?Sized, Drawer: RasterDrawer + ?Sized> Glyph<Drawer> for &'_ T {
    fn draw_to(&self, drawer: &mut Drawer, offset: Vector2i) {
        (**self).draw_to(drawer, offset)
    }
}

impl<T: GlyphPlacer + ?Sized> GlyphPlacer for &'_ mut T {
    type Glyph = T::Glyph;

    fn peek_last(&mut self) -> Option<&mut (Vector2i, Self::Glyph)> {
        (**self).peek_last()
    }

    fn pop_last(&mut self) -> bool {
        (**self).pop_last()
    }

    fn place(&mut self, offset: Vector2i, glyph: Self::Glyph)
    where
        Self::Glyph: Sized,
    {
        (**self).place(offset, glyph)
    }
}

/// Provides the necessary context for placing glyphs for a line of text. This specifies which
/// direction the text is going in, and maintains the "pen" position for the text.
pub struct TextWriter<T: ?Sized> {
    /// The direction the text is going.
    pub dir: Dir2i,

    /// The "pen" position for the text. This is a point on the baseline that marks where the
    /// next character should start. It is represents as a [`Vector2`] instead of a [`Vector2i`]
    /// to allow sub-pixel offsets which may be relevant for rasterization.
    /// Note that the baseline is always below (-Y) or to the left (-X) of the text, regardless of
    /// which direction it is going.
    pub pen: Vector2,

    /// The size of the line of text. For horizontal text, this will be its height, and for
    /// vertical text it will be its width.
    pub line_size: f32,

    /// The underlying [`GlyphPlacer`] for this [`TextWriter`].
    pub target: T,
}

impl<T: GlyphPlacer> TextWriter<T> {
    /// Writes a string to this [`TextWriter`] using the given font.
    pub fn write<'font, F: Font<Glyph<'font> = T::Glyph> + ?Sized>(
        &mut self,
        font: &'font F,
        str: &str,
    ) {
        font.write_to(self, &mut str.chars().peekable())
    }
}

/// A [`GlyphPlacer`] which immediately draws placed glyphs. This type of placer does not
/// allow access to previously-placed glyphs. Thus, ligatures are not supported.
#[repr(transparent)]
pub struct ImmediateDrawPlacer<G: Glyph<Drawer> + ?Sized, Drawer: RasterDrawer + ?Sized> {
    _marker: PhantomData<fn(&G)>,
    target: Drawer,
}

impl<G: Glyph<Drawer> + ?Sized, Drawer: RasterDrawer + ?Sized> ImmediateDrawPlacer<G, Drawer> {
    /// Constructs a reference to an [`ImmediateDrawPlacer`] from a reference to a drawer.
    pub fn from_mut(target: &mut Drawer) -> &mut ImmediateDrawPlacer<G, Drawer> {
        unsafe { std::mem::transmute(target) }
    }
}

impl<G: Glyph<Drawer> + ?Sized, Drawer: RasterDrawer + ?Sized> GlyphPlacer
    for ImmediateDrawPlacer<G, Drawer>
{
    type Glyph = G;

    fn peek_last(&mut self) -> Option<&mut (Vector2i, G)> {
        None
    }

    fn pop_last(&mut self) -> bool {
        false
    }

    fn place(&mut self, offset: Vector2i, glyph: G)
    where
        Self::Glyph: Sized,
    {
        glyph.draw_to(&mut self.target, offset)
    }
}

/// Provides extension methods for [`RasterDrawer`] related to text drawing.
pub trait TextDrawer: RasterDrawer {
    /// Draws a string of text to this [`TextDrawer`] without buffering [`Glyph`]s. This is
    /// very fast, but does not allow for alignment, wrapping or advanced text rendering
    /// features such as ligatures.
    fn draw_text_immediate<'font, F: Font>(
        &mut self,
        font: &'font F,
        dir: Dir2i,
        offset: Vector2i,
        str: &str,
    ) where
        F::Glyph<'font>: Glyph<Self>,
    {
        let mut writer = TextWriter {
            dir,
            pen: vec2(offset.x as Scalar, offset.y as Scalar),
            line_size: 0.0,
            target: ImmediateDrawPlacer::from_mut(self),
        };
        writer.write(font, str);
    }

    /// Shortcut for [`TextDrawer::draw_text_immediate`] for left-to-right text.
    fn draw_text_immediate_ltr<'font, F: Font>(
        &mut self,
        font: &'font F,
        offset: Vector2i,
        str: &str,
    ) where
        F::Glyph<'font>: Glyph<Self>,
    {
        self.draw_text_immediate(font, Dir2i::PosX, offset, str)
    }

    /// Draws text to this [`TextDrawer`] from a [`TextBuffer`].
    fn draw_text_buffer<G: Glyph<Self>>(&mut self, offset: Vector2i, buffer: &TextBuffer<G>) {
        for (glyph_offset, glyph) in buffer.glyphs() {
            glyph.draw_to(self, offset + glyph_offset)
        }
    }
}

impl<T: RasterDrawer + ?Sized> TextDrawer for T {}

/// A buffer for positioned [`Glyph`]s.
pub struct TextBuffer<G> {
    glyphs: Vec<(Vector2i, G)>,
}

impl<G> TextBuffer<G> {
    /// Creates a new initially-empty [`TextBuffer`].
    pub fn new() -> Self {
        Self { glyphs: Vec::new() }
    }

    /// Iterates over the glyphs in this buffer.
    pub fn glyphs(&self) -> impl Iterator<Item = (Vector2i, &G)> {
        self.glyphs.iter().map(|(offset, glyph)| (*offset, glyph))
    }
}

impl<G> Default for TextBuffer<G> {
    fn default() -> Self {
        Self::new()
    }
}

impl<G> GlyphPlacer for TextBuffer<G> {
    type Glyph = G;

    fn peek_last(&mut self) -> Option<&mut (Vector2i, G)> {
        self.glyphs.last_mut()
    }

    fn pop_last(&mut self) -> bool {
        self.glyphs.pop().is_some()
    }

    fn place(&mut self, offset: Vector2i, glyph: G)
    where
        Self::Glyph: Sized,
    {
        self.glyphs.push((offset, glyph))
    }
}
