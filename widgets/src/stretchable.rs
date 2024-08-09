use std::num::NonZeroU32;
use uial::drawer::{Image, ImageDrawer, ImageHandle};
use uial::prelude::*;

/// Annotates an [`Image`] with additional information that allows it to be stretched to
/// arbitrary sizes (larger than the source image).
pub struct StretchableImage<H: ImageHandle> {
    source: Image<H>,
    pattern: BandPattern,
}

/// Describes a horizontal or vertical region of the source image of a [`Stretchable`] that has
/// its own expansion properties.
#[derive(Copy, Clone)]
pub struct Band {
    /// The number of pixels in the source image covered by this band.
    pub size: NonZeroU32,

    /// The expansion properties of this band.
    pub mode: BandMode,
}

/// Describes the expansion properties of a horizontal or vertical region of the source image of a
/// [`Stretchable`].
#[derive(Copy, Clone)]
pub enum BandMode {
    /// The band can't be expanded.
    Fixed,

    /// The band can be expanded by repeating along the band axis.
    Tiling {
        /// The relative size of this band in relation to other stretchable bands on the
        /// same axis.
        weight: NonZeroU32,
    },
}

/// Describes the expansion properties of an entire [`Stretchable`] image along both axes.
#[derive(Clone)]
pub struct BandPattern {
    bands_x: Vec<Band>,
    bands_y: Vec<Band>,
    total_weight_x: NonZeroU32,
    total_weight_y: NonZeroU32,
    min_size: Size2i,
}

impl BandPattern {
    /// Constructs a new [`BandPattern`] from the given bands.
    ///
    /// Returns [`None`] if the total weight of all bands on some axis is zero, preventing
    /// expansion on that axis.
    pub fn new(bands_x: Vec<Band>, bands_y: Vec<Band>) -> Option<Self> {
        let total_weight_x = bands_x
            .iter()
            .map(|b| match b.mode {
                BandMode::Fixed => 0,
                BandMode::Tiling { weight } => weight.get(),
            })
            .sum();
        let total_weight_y = bands_y
            .iter()
            .map(|b| match b.mode {
                BandMode::Fixed => 0,
                BandMode::Tiling { weight } => weight.get(),
            })
            .sum();
        let total_weight_x = NonZeroU32::new(total_weight_x)?;
        let total_weight_y = NonZeroU32::new(total_weight_y)?;
        let min_size = size2i(
            bands_x
                .iter()
                .map(|b| match b.mode {
                    BandMode::Fixed => b.size.get(),
                    BandMode::Tiling { .. } => 0,
                })
                .sum(),
            bands_y
                .iter()
                .map(|b| match b.mode {
                    BandMode::Fixed => b.size.get(),
                    BandMode::Tiling { .. } => 0,
                })
                .sum(),
        );
        Some(Self {
            bands_x,
            bands_y,
            total_weight_x,
            total_weight_y,
            min_size,
        })
    }

    
    /// Gets the minimum size this pattern can be stretched to.
    pub fn min_size(&self) -> Size2i {
        self.min_size
    }

    /// Gets the size of the source image this pattern can be used with.
    pub fn size(&self) -> Size2i {
        let x = self.bands_x.iter().map(|b| b.size.get()).sum();
        let y = self.bands_y.iter().map(|b| b.size.get()).sum();
        size2i(x, y)
    }
}

impl<H: ImageHandle> StretchableImage<H> {
    /// Constructs a new [`Stretchable`] from the given source image and [`BandPattern`].
    ///
    /// Returns [`None`] if the size of the source image does not match the size of the pattern.
    pub fn new(source: Image<H>, pattern: BandPattern) -> Option<Self> {
        if source.size() != pattern.size() {
            return None;
        }
        Some(Self { source, pattern })
    }

    /// Gets the minimum size of this image.
    pub fn min_size(&self) -> Size2i {
        self.pattern.min_size()
    }

    /// Draws the expandable image to the given drawer with the given size.
    pub fn draw_to<Drawer: ImageDrawer<H::Source> + ?Sized>(
        &self,
        size: Size2i,
        drawer: &mut Drawer,
        paint: Paint,
        trans: Ortho2i,
    ) {
        let source = self.source.to_source();
        let excess_size = size - self.min_size();
        for band_y in draw_bands(
            &self.pattern.bands_y,
            self.pattern.total_weight_y,
            excess_size.y,
        ) {
            for band_x in draw_bands(
                &self.pattern.bands_x,
                self.pattern.total_weight_x,
                excess_size.x,
            ) {
                let cell_source = source
                    .view(Box2i::from_min_size(
                        vec2i(band_x.source_offset as i32, band_y.source_offset as i32),
                        size2i(band_x.source_size, band_y.source_size),
                    ))
                    .unwrap();
                let cell_trans = Ortho2i::translate(vec2i(
                    band_x.target_offset as i32,
                    band_y.target_offset as i32,
                )) * Ortho2i::scale(vec2i(
                    band_x.target_scale as i32,
                    band_y.target_scale as i32,
                ));
                drawer.draw_image(cell_source, paint, trans * cell_trans);
            }
        }
    }
}

/// Gets the [`DrawBand`]s along one axis required to draw a [`Stretchable`] that is stretched by
/// the given size.
fn draw_bands(
    bands: &[Band],
    total_weight: NonZeroU32,
    excess_size: u32,
) -> impl Iterator<Item = DrawBand> + '_ {
    let mut rem_excess_size = excess_size;
    let mut rem_weight = total_weight.get();
    let mut rem_bands = bands.iter();
    let mut source_offset = 0;
    let mut target_offset = 0;
    let mut source_size = 0;
    let mut next_target_offset = 0;
    let mut target_scale = 1;
    std::iter::from_fn(move || {
        if target_offset >= next_target_offset {
            source_offset += source_size;
            let band = rem_bands.next()?;
            source_size = band.size.get();
            let (target_size, n_target_scale) = match band.mode {
                BandMode::Fixed => (source_size, 1),
                BandMode::Tiling { weight } => {
                    let weight = weight.get();
                    let target_size = rem_excess_size * weight / rem_weight;
                    rem_excess_size -= target_size;
                    rem_weight -= weight;
                    (target_size, if source_size == 1 { target_size } else { 1 })
                }
            };
            next_target_offset = target_offset + target_size;
            target_scale = n_target_scale;
        }
        let res = DrawBand {
            source_offset,
            source_size,
            target_offset,
            target_scale,
        };
        // TODO: Clip last band
        target_offset += source_size * target_scale;
        Some(res)
    })
}

/// Describes a drawing operation required to draw a region along one axis of a [`Stretchable`].
struct DrawBand {
    /// The offset of the start of this band in the source image.
    source_offset: u32,

    /// The size of this band in the source image.
    source_size: u32,

    /// The offset of the drawn band in the target drawer.
    target_offset: u32,

    /// The scale factor applied to the drawn band.
    target_scale: u32,
}
