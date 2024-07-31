use super::*;
use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::RwLock;

/// A [`Font`] which produces glyphs from a [`ttf_parser::Face`].
pub struct ImageTTFont<M: ImageManager, Family: Borrow<ImageTTFontFamily<M>>> {
    _marker: PhantomData<M>,
    pixels_per_4_em: u16,
    paint: Paint,
    family: Family,
}

/// A wrapper over a [`ttf_parser::Face`] which rasterizes and caches glyphs using
/// an [`ImageManager`].
pub struct ImageTTFontFamily<M: ImageManager> {
    data_ptr: *mut [u8],
    // This lifetime is a lie. `face` refers to data in `data_ptr`.
    face: MaybeUninit<FaceInfo<'static>>,
    image_manager: M,
    // TODO: Evict old entries from cache
    glyph_images: RwLock<HashMap<ImageTTGlyphKey, (M::Image, Vector2i)>>,
}

/// Contains relevant information about the [`ttf_parser::Face`] for a [`ImageTTFont`].
struct FaceInfo<'a> {
    source: ttf_parser::Face<'a>,
    glyphs: Box<[GlyphInfo]>,
    kern_subtables_h: Vec<ttf_parser::kern::Subtable<'a>>,
    x_height: i16,
    cap_height: i16,
}

/// Contains relevant information about a glyph in a [`ttf_parser::Face`].
struct GlyphInfo {
    hor_advance: u16,
    render_info: Option<GlyphRenderInfo>,
}

/// Contains information about how a glyph is to be rendered.
struct GlyphRenderInfo {
    render_type: GlyphRenderType,
    bounding_box: ttf_parser::Rect,
}

/// Describes how a glyph is to be rendered.
#[derive(Clone, Copy)]
enum GlyphRenderType {
    Raster,
    Vector,
}

/// A [`Glyph`] for an [`ImageTTFont`].
#[derive(Clone, Copy)]
pub struct ImageTTGlyph<'a, M: ImageManager> {
    family: &'a ImageTTFontFamily<M>,
    key: ImageTTGlyphKey,
    paint: Paint,
}

/// Encapsulates the properties of [`ImageTTGlyph`] that are used to identify a rasterized image
/// of a glyph in a cache.
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct ImageTTGlyphKey {
    glyph_id: ttf_parser::GlyphId,
    pixels_per_4_em: u16,
    sub_pixel_x: u8,
    sub_pixel_y: u8,
}

impl<'a> FaceInfo<'a> {
    /// Constructs a `FaceInfo` for the given face.
    pub fn new(face: ttf_parser::Face<'a>) -> Self {
        let glyphs: Box<[GlyphInfo]> = (0..face.number_of_glyphs())
            .map(|i| {
                let glyph_id = ttf_parser::GlyphId(i);
                GlyphInfo {
                    hor_advance: face.glyph_hor_advance(glyph_id).unwrap_or(0),
                    render_info: {
                        if let Some(image) = face.glyph_raster_image(glyph_id, u16::MAX) {
                            if image.width > 0 && image.height > 0 {
                                let units_per_pixel =
                                    (face.units_per_em() / image.pixels_per_em) as i16;
                                Some(GlyphRenderInfo {
                                    render_type: GlyphRenderType::Raster,
                                    bounding_box: ttf_parser::Rect {
                                        x_min: image.x * units_per_pixel,
                                        y_min: image.y * units_per_pixel,
                                        x_max: (image.width as i16) * units_per_pixel,
                                        y_max: (image.height as i16) * units_per_pixel,
                                    },
                                })
                            } else {
                                None
                            }
                        } else {
                            face.glyph_bounding_box(glyph_id)
                                .map(|bounding_box| GlyphRenderInfo {
                                    render_type: GlyphRenderType::Vector,
                                    bounding_box,
                                })
                        }
                    },
                }
            })
            .collect();
        let kern_subtables_h = face
            .tables()
            .kern
            .iter()
            .flat_map(|kern| kern.subtables.into_iter())
            .filter(|s| s.horizontal)
            .collect();
        let x_height = face.x_height().unwrap_or_else(|| {
            ['x', 'v', 'w', 'z']
                .into_iter()
                .filter_map(|ch| face.glyph_index(ch))
                .filter_map(|id| glyphs[usize::from(id.0)].render_info.as_ref())
                .map(|r| r.bounding_box.y_max)
                .max()
                .unwrap_or(0)
        });
        let cap_height = face.capital_height().unwrap_or_else(|| {
            ['H', 'K', 'L', 'T', 'I']
                .into_iter()
                .filter_map(|ch| face.glyph_index(ch))
                .filter_map(|id| glyphs[usize::from(id.0)].render_info.as_ref())
                .map(|r| r.bounding_box.y_max)
                .max()
                .unwrap_or(0)
        });
        Self {
            source: face,
            glyphs,
            kern_subtables_h,
            x_height,
            cap_height,
        }
    }
}

impl<M: ImageManager> ImageTTFontFamily<M> {
    /// Tries constructing an [`ImageTTFontFamily`] from the given font data and [`ImageManager`].
    pub fn new(
        font_data: Box<[u8]>,
        image_manager: M,
    ) -> Result<Self, ttf_parser::FaceParsingError> {
        let data_ptr = Box::into_raw(font_data);
        let data_ref = unsafe { &*data_ptr };
        let face = ttf_parser::Face::parse(data_ref, 0)?;
        Ok(Self {
            data_ptr,
            face: MaybeUninit::new(FaceInfo::new(face)),
            image_manager,
            glyph_images: RwLock::new(HashMap::new()),
        })
    }

    /// Gets the [`FaceInfo`] for this font family.
    fn face(&self) -> &FaceInfo {
        unsafe { self.face.assume_init_ref() }
    }
}

impl<M: ImageManager> Drop for ImageTTFontFamily<M> {
    fn drop(&mut self) {
        unsafe {
            self.face.assume_init_drop();
            drop(Box::from_raw(self.data_ptr));
        }
    }
}

/// Contains extension methods for values which can provide an [`ImageTTFontFamily`].
pub trait BorrowImageTTFontFamily<M: ImageManager>: Sized + Borrow<ImageTTFontFamily<M>> {
    /// Constructs a [`Font`] based on this font family, given the conversion factor between
    /// *em*s and pixels. If the font is a bitmap font, this will use the closest available size.
    /// The [`Font`] will initially be black, but the color can be updated using
    /// [`ImageTTFont::with_paint`].
    fn with_em_scale(self, pixels_per_em: f32) -> ImageTTFont<M, Self> {
        let mut pixels_per_4_em = (4.0 * pixels_per_em).clamp(0.0, u16::MAX as f32).round() as u16;
        let face: &FaceInfo = self.borrow().face();
        let tables = face.source.tables();
        if tables.cbdt.is_some() || tables.ebdt.is_some() {
            // This is a bitmap font. Try to find the closest matching size
            let pixels_per_em = pixels_per_4_em / 4;
            for i in 0..face.source.number_of_glyphs() {
                let glyph_id = ttf_parser::GlyphId(i);
                if let Some(image) = face.source.glyph_raster_image(glyph_id, pixels_per_em) {
                    pixels_per_4_em = image.pixels_per_em * 4;
                    break;
                }
            }
        }
        ImageTTFont {
            _marker: PhantomData,
            pixels_per_4_em,
            paint: Paint::new(0.0, 0.0, 0.0, 1.0),
            family: self,
        }
    }

    /// Constructs a [`Font`] based on this font family, given the full height of the text in
    /// pixels. If the font is a bitmap font, this will use the closest available size. The
    /// [`Font`] will initially be black, but the color can be updated using
    /// [`ImageTTFont::with_paint`].
    fn with_height(self, height: f32) -> ImageTTFont<M, Self> {
        let face: &FaceInfo = self.borrow().face();
        let height_in_units = face.source.ascender() - face.source.descender();
        let units_per_em = face.source.units_per_em();
        self.with_em_scale(height * (units_per_em as f32) / (height_in_units as f32))
    }

    /// Constructs a [`Font`] based on this font family, given the median height of the text in
    /// pixels. If the font is a bitmap font, this will use the closest available size. Returns
    /// [`None`] if this information is not available. The [`Font`] will initially be black, but
    /// the color can be updated using [`ImageTTFont::with_paint`].
    fn with_x_height(self, height: f32) -> ImageTTFont<M, Self> {
        let face: &FaceInfo = self.borrow().face();
        let height_in_units = face.x_height;
        let units_per_em = face.source.units_per_em();
        self.with_em_scale(height * (units_per_em as f32) / (height_in_units as f32))
    }

    /// Constructs a [`Font`] based on this font family, given the capital height of the text in
    /// pixels. If the font is a bitmap font, this will use the closest available size. Returns
    /// [`None`] if this information is not available. The [`Font`] will initially be black, but
    /// the color can be updated using [`ImageTTFont::with_paint`].
    fn with_cap_height(self, height: f32) -> ImageTTFont<M, Self> {
        let face: &FaceInfo = self.borrow().face();
        let height_in_units = face.cap_height;
        let units_per_em = face.source.units_per_em();
        self.with_em_scale(height * (units_per_em as f32) / (height_in_units as f32))
    }
}

impl<M: ImageManager, T: Borrow<ImageTTFontFamily<M>>> BorrowImageTTFontFamily<M> for T {}

impl<M: ImageManager, Family: Borrow<ImageTTFontFamily<M>> + Copy> Copy for ImageTTFont<M, Family> {}

impl<M: ImageManager, Family: Borrow<ImageTTFontFamily<M>> + Clone> Clone
    for ImageTTFont<M, Family>
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            pixels_per_4_em: self.pixels_per_4_em,
            paint: self.paint,
            family: self.family.clone(),
        }
    }
}

impl<M: ImageManager, Family: Borrow<ImageTTFontFamily<M>>> ImageTTFont<M, Family> {
    /// Updates the [`Paint`] this font is drawn with.
    pub fn with_paint(mut self, paint: Paint) -> Self {
        self.paint = paint;
        self
    }
}

impl<M: ImageManager, Family: Borrow<ImageTTFontFamily<M>>> Font for ImageTTFont<M, Family> {
    type Glyph<'font> = ImageTTGlyph<'font, M> where Self: 'font;
    fn write_to<'font>(
        &'font self,
        writer: &mut TextWriter<impl GlyphPlacer<Glyph = ImageTTGlyph<'font, M>>>,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) {
        let family = self.family.borrow();
        let mut last_glyph_id = writer
            .target
            .peek_last()
            .filter(|(_, glyph)| std::ptr::eq(glyph.family, family))
            .map(|(_, glyph)| glyph.key.glyph_id);
        let mut any_glyph = false;
        let face = family.face();
        #[allow(clippy::while_let_on_iterator)]
        while let Some(ch) = chars.next() {
            // TODO: Glyph fallback
            // TODO: Multi-char glyphs
            let glyph_id = face.source.glyph_index(ch).expect("Missing glyph");
            let glyph_info = &face.glyphs[glyph_id.0 as usize];
            match writer.dir {
                Dir2i::PosX => {
                    // Place glyph if it is visible
                    if glyph_info.render_info.is_some() {
                        let (offset_x, sub_pixel_x) = quantize_pixel_offset(writer.pen.x);
                        let (offset_y, sub_pixel_y) = quantize_pixel_offset(writer.pen.y);
                        writer.target.place(
                            vec2i(offset_x, offset_y),
                            ImageTTGlyph {
                                family,
                                key: ImageTTGlyphKey {
                                    glyph_id,
                                    pixels_per_4_em: self.pixels_per_4_em,
                                    sub_pixel_x,
                                    sub_pixel_y,
                                },
                                paint: self.paint,
                            },
                        );
                    }

                    // Apply advance
                    let kerning = last_glyph_id
                        .and_then(|prev_glyph_id| {
                            face.kern_subtables_h
                                .iter()
                                .find_map(|s| s.glyphs_kerning(prev_glyph_id, glyph_id))
                        })
                        .unwrap_or(0);
                    let advance = i32::from(glyph_info.hor_advance) + i32::from(kerning);
                    writer.pen.x += ((advance * i32::from(self.pixels_per_4_em)) as Scalar)
                        / (4.0 * face.source.units_per_em() as Scalar);
                    last_glyph_id = Some(glyph_id);
                    any_glyph = true;
                }
                _ => todo!(),
            }
        }

        // Update `line_size`
        if any_glyph {
            writer.line_size = writer.line_size.max(
                ((i32::from(face.cap_height) * i32::from(self.pixels_per_4_em)) as Scalar)
                    / (4.0 * face.source.units_per_em() as Scalar),
            );
        }
    }
}

impl<M: ImageManager, Drawer: ImageDrawer<M::Store> + ?Sized> Glyph<Drawer>
    for ImageTTGlyph<'_, M>
{
    fn draw_to(&self, drawer: &mut Drawer, offset: Vector2i) {
        // Check cache
        {
            let glyph_cache = self.family.glyph_images.read().unwrap();
            if let Some((image, image_offset)) = glyph_cache.get(&self.key) {
                drawer.draw_image(
                    image.as_source(),
                    self.paint,
                    Similarity2i::translate(offset + *image_offset),
                );
                return;
            }
        }

        // Rasterize glyph while cache is unlocked
        let (image, image_offset) = rasterize(self.family.face(), &self.key);

        // Lock to prevent loading the image multiple times in a race condition.
        let mut glyph_cache = self.family.glyph_images.write().unwrap();
        match glyph_cache.entry(self.key) {
            Entry::Occupied(entry) => {
                let (image, image_offset) = entry.get();
                drawer.draw_image(
                    image.as_source(),
                    self.paint,
                    Similarity2i::translate(offset + *image_offset),
                );
            }
            Entry::Vacant(entry) => {
                let image = self.family.image_manager.load_image(image.into());
                let (image, _) = entry.insert((image, image_offset));
                drawer.draw_image(
                    image.as_source(),
                    self.paint,
                    Similarity2i::translate(offset + image_offset),
                );
            }
        }
    }
}

/// Rasterizes a glyph.
fn rasterize(face: &FaceInfo, key: &ImageTTGlyphKey) -> (image::GrayAlphaImage, Vector2i) {
    let render_info = face.glyphs[usize::from(key.glyph_id.0)]
        .render_info
        .as_ref()
        .unwrap();
    match render_info.render_type {
        GlyphRenderType::Raster => {
            let pixels_per_em = key.pixels_per_4_em / 4;
            let source_image = face
                .source
                .glyph_raster_image(key.glyph_id, pixels_per_em)
                .unwrap();
            if source_image.pixels_per_em != pixels_per_em {
                // TODO: Scale raster image to different sizes
                todo!()
            }
            let offset = vec2i(i32::from(source_image.x), i32::from(source_image.y));
            let mut image = image::GrayAlphaImage::new(
                u32::from(source_image.width),
                u32::from(source_image.height),
            );
            use ttf_parser::RasterImageFormat::*;
            match source_image.format {
                PNG => todo!(),
                BitmapMono => todo!(),
                BitmapMonoPacked => {
                    let mut bits = source_image.data.iter().flat_map(|byte| {
                        core::array::from_fn::<bool, 8, _>(|i| *byte & (1 << (7 - i)) != 0)
                    });
                    for pixel in image.pixels_mut() {
                        let bit = bits.next().expect("not enough bitmap data");
                        *pixel = [255, u8::from(bit) * 255].into();
                    }
                }
                BitmapGray2 => todo!(),
                BitmapGray2Packed => todo!(),
                BitmapGray4 => todo!(),
                BitmapGray4Packed => todo!(),
                BitmapGray8 => todo!(),
                BitmapPremulBgra32 => todo!(),
            };
            (image, offset)
        }
        GlyphRenderType::Vector => {
            let bbox = render_info.bounding_box;
            let scale_factor =
                (key.pixels_per_4_em as f32) / (4.0 * face.source.units_per_em() as f32);
            let sub_pixel_x = unquantize_pixel_offset(key.sub_pixel_x);
            let sub_pixel_y = unquantize_pixel_offset(key.sub_pixel_y);
            let min_x = (bbox.x_min as f32 * scale_factor + sub_pixel_x).floor() as i32;
            let min_y = (bbox.y_min as f32 * scale_factor + sub_pixel_y).floor() as i32;
            let max_x = (bbox.x_max as f32 * scale_factor + sub_pixel_x).ceil() as i32;
            let max_y = (bbox.y_max as f32 * scale_factor + sub_pixel_y).ceil() as i32;
            let offset = vec2i(min_x, min_y);
            let width = max_x - min_x;
            let height = max_y - min_y;
            let mut rasterizer = OutlineRasterizer {
                scale_x: scale_factor,
                scale_y: -scale_factor,
                offset_x: sub_pixel_x - min_x as f32,
                offset_y: max_y as f32 - sub_pixel_y,
                start: None,
                last: Default::default(),
                target: ab_glyph_rasterizer::Rasterizer::new(width as usize, height as usize),
            };
            face.source.outline_glyph(key.glyph_id, &mut rasterizer);

            // Explicitly close the glyph if it has not yet been closed
            ttf_parser::OutlineBuilder::close(&mut rasterizer);

            // Copy rasterized data to an image
            let mut image = image::GrayAlphaImage::new(width as u32, height as u32);
            rasterizer.target.for_each_pixel_2d(|x, y, alpha| {
                *image.get_pixel_mut(x, y) = [255, (alpha * 255.0) as u8].into();
            });
            (image, offset)
        }
    }
}

/// A [`ttf_parser::OutlineBuilder`] which forwards curves to an
/// [`ab_glyph_rasterizer::Rasterizer`].
struct OutlineRasterizer {
    scale_x: f32,
    scale_y: f32,
    offset_x: f32,
    offset_y: f32,
    start: Option<ab_glyph_rasterizer::Point>,
    last: ab_glyph_rasterizer::Point,
    target: ab_glyph_rasterizer::Rasterizer,
}

impl OutlineRasterizer {
    /// Converts a set of coordinates into an [`ab_glyph_rasterizer::Point`] using the
    /// transformation specified in this [`OutlineRasterizer`].
    fn point(&self, x: f32, y: f32) -> ab_glyph_rasterizer::Point {
        ab_glyph_rasterizer::point(
            self.scale_x * x + self.offset_x,
            self.scale_y * y + self.offset_y,
        )
    }
}

impl ttf_parser::OutlineBuilder for OutlineRasterizer {
    fn move_to(&mut self, x: f32, y: f32) {
        self.last = self.point(x, y);
        self.start = Some(self.last);
    }

    fn line_to(&mut self, x: f32, y: f32) {
        let p1 = self.point(x, y);
        self.target.draw_line(self.last, p1);
        self.last = p1;
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32) {
        let p1 = self.point(x1, y1);
        let p2 = self.point(x2, y2);
        self.target.draw_quad(self.last, p1, p2);
        self.last = p2;
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32) {
        let p1 = self.point(x1, y1);
        let p2 = self.point(x2, y2);
        let p3 = self.point(x3, y3);
        self.target.draw_cubic(self.last, p1, p2, p3);
        self.last = p3;
    }

    fn close(&mut self) {
        if let Some(start) = self.start.take() {
            self.target.draw_line(self.last, start);
        }
    }
}

/// The number of discrete sub-pixel offset values per axis in a `ImageTTGlyphKey`.
const TICKS_PER_PIXEL: u8 = 4;

/// Quantizes a pixel offset into an integer part and a sub-pixel fractional part.
fn quantize_pixel_offset(offset: f32) -> (i32, u8) {
    let ticks = (offset * TICKS_PER_PIXEL as f32).round();
    let pixels = (ticks / (TICKS_PER_PIXEL as f32)).floor() as i32;
    let sub_pixels = (ticks as i32 - pixels * TICKS_PER_PIXEL as i32) as u8;
    (pixels, sub_pixels)
}

/// Approximate inverse of the fractional part of `quantize_pixel_offset`.
fn unquantize_pixel_offset(sub_pixel_offset: u8) -> f32 {
    (sub_pixel_offset as f32) / (TICKS_PER_PIXEL as f32)
}
