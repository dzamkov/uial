mod image_tt;
use super::*;
pub use image_tt::*;
use std::iter::Peekable;
use std::marker::PhantomData;
use uial_geometry::Dir2i;

/// Describes a method of converting arbitrary strings of text into a form that can be drawn
/// to a [`RasterDrawer`]. This fully captures all information about the appearance/style of the
/// text, including size, color and weight.
pub trait FontBase {
    /// Identifies a type of glyph that can be produced by this [`Font`].
    type Glyph: ?Sized;

    /// Converts a stream of characters into glyphs using a [`TextWriter`].
    fn write_to(
        &self,
        writer: &mut TextWriter<impl GlyphPlacer<Glyph = Self::Glyph>>,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    );
}

/// Describes a method of converting arbitrary strings of text into a form that can be drawn
/// to a given type of `Drawer`. This fully captures all information about the appearance/style of
/// the text, including size, color and weight.
pub trait Font<Drawer: RasterDrawer + ?Sized>: FontBase {
    /// Draws a glyph to the given drawer at the given offset.
    fn draw_glyph_to(&self, drawer: &mut Drawer, glyph: &Self::Glyph, offset: Vector2i);
}

/// An interface for placing glyphs.
pub trait GlyphPlacer {
    /// The type of glyph accepted by this [`GlyphPlacer`].
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

impl<T: FontBase + ?Sized> FontBase for &'_ T {
    type Glyph = T::Glyph;
    fn write_to(
        &self,
        writer: &mut TextWriter<impl GlyphPlacer<Glyph = Self::Glyph>>,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) {
        (**self).write_to(writer, chars);
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
    pub fn write<F: FontBase<Glyph = T::Glyph> + ?Sized>(&mut self, font: &F, str: &str) {
        font.write_to(self, &mut str.chars().peekable())
    }
}

/// A [`GlyphPlacer`] which immediately draws placed glyphs. This type of placer does not
/// allow access to previously-placed glyphs. Thus, ligatures are not supported.
pub struct ImmediateDrawPlacer<'a, F: Font<Drawer>, Drawer: RasterDrawer + ?Sized> {
    font: &'a F,
    target: &'a mut Drawer,
}

impl<F: Font<Drawer>, Drawer: RasterDrawer + ?Sized> GlyphPlacer
    for ImmediateDrawPlacer<'_, F, Drawer>
{
    type Glyph = F::Glyph;

    fn peek_last(&mut self) -> Option<&mut (Vector2i, F::Glyph)> {
        None
    }

    fn pop_last(&mut self) -> bool {
        false
    }

    fn place(&mut self, offset: Vector2i, glyph: F::Glyph)
    where
        Self::Glyph: Sized,
    {
        self.font.draw_glyph_to(self.target, &glyph, offset)
    }
}

/// Provides extension methods for [`RasterDrawer`] related to text drawing.
pub trait TextDrawer: RasterDrawer {
    /// Draws a string of text to this [`TextDrawer`] without buffering [`Glyph`]s. This is
    /// very fast, but does not allow for alignment, wrapping or advanced text rendering
    /// features such as ligatures.
    fn draw_text_immediate<F: Font<Self>>(
        &mut self,
        font: &F,
        dir: Dir2i,
        offset: Vector2i,
        str: &str,
    ) {
        let mut writer = TextWriter {
            dir,
            pen: vec2(offset.x as Scalar, offset.y as Scalar),
            line_size: 0.0,
            target: ImmediateDrawPlacer { font, target: self },
        };
        writer.write(font, str);
    }

    /// Shortcut for [`TextDrawer::draw_text_immediate`] for left-to-right text.
    fn draw_text_immediate_ltr<F: Font<Self>>(&mut self, font: &F, offset: Vector2i, str: &str) {
        self.draw_text_immediate(font, Dir2i::PosX, offset, str)
    }

    /// Draws text to this [`TextDrawer`] from a [`TextBuffer`].
    fn draw_text_buffer<F: Font<Self> + ?Sized>(
        &mut self,
        font: &F,
        offset: Vector2i,
        buffer: &TextBuffer<F::Glyph>,
    ) where
        F::Glyph: Sized,
    {
        for (glyph_offset, glyph) in buffer.glyphs() {
            font.draw_glyph_to(self, glyph, offset + glyph_offset)
        }
    }
}

impl<T: RasterDrawer + ?Sized> TextDrawer for T {}

/// A buffer for positioned glyphs.
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

    /// Clears this buffer.
    pub fn clear(&mut self) {
        self.glyphs.clear()
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
