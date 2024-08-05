mod polyline;
mod text;

use super::*;
pub use polyline::*;
use std::rc::Rc;
use std::sync::Arc;
pub use text::*;
use uial_geometry::{Ortho2i, Similarity2};

/// The typical color type for use with drawing functions.
pub type Color = palette::LinSrgb<f32>;

/// Shortcut for constructing a [`Color`] from its sRGB components.
pub fn srgb(r: f32, g: f32, b: f32) -> Color {
    palette::Srgb::new(r, g, b).into_linear()
}

/// A [`Color`] combined with an alpha value specifying opacity.
pub type Paint = palette::LinSrgba<f32>;

/// Shortcut for constructing a [`Paint`] from its sRGB components.
pub fn srgba(r: f32, g: f32, b: f32, a: f32) -> Paint {
    palette::Srgba::new(r, g, b, a).into_linear()
}

/// An interface for drawing lines and polygons on a continuous two-dimensional surface.
pub trait VectorDrawer {
    /// Gets a [`Box2`] which covers all space that is "visible" (i.e. not ignored by the drawer).
    fn visible_bounds(&self) -> Box2 {
        Box2::all()
    }

    /// Begins drawing a poly-arc.
    fn draw_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcDrawer<'_>;

    /// The type of [`PolyarcBuilder`] produced by a call to [`VectorDrawer::draw_polyarc`].
    type PolyarcDrawer<'a>: PolyarcBuilder
    where
        Self: 'a;

    /// Begins filling an area enclosed by a poly-arc. The poly-arc must be built in
    /// counter-clockwise order and end at the start point.
    fn fill_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcFiller<'_>;

    /// The type of [`PolyarcBuilder`] produced by a call to [`VectorDrawer::fill_polyarc`].
    type PolyarcFiller<'a>: PolyarcBuilder
    where
        Self: 'a;

    /// Draws a line between the given points.
    fn draw_line(&mut self, paint: Paint, a: Vector2, b: Vector2) {
        self.draw_polyarc(paint, a).line_to(b)
    }

    /// Draws the outline of a circle.
    fn draw_circle(&mut self, paint: Paint, center: Vector2, radius: Scalar) {
        let mut polyarc = self.draw_polyarc(paint, center + vec2(radius, 0.0));
        polyarc.circular_arc_to(0.5, center - vec2(radius, 0.0));
        polyarc.circular_arc_to(0.5, center + vec2(radius, 0.0));
    }

    /// Draws a filled circle.
    fn fill_circle(&mut self, paint: Paint, center: Vector2, radius: Scalar) {
        let mut polyarc = self.fill_polyarc(paint, center + vec2(radius, 0.0));
        polyarc.circular_arc_to(0.5, center - vec2(radius, 0.0));
        polyarc.circular_arc_to(0.5, center + vec2(radius, 0.0));
    }
}

/// An interface for drawing on a discrete (i.e. pixel-based) surface.
pub trait RasterDrawer {
    /// Draws a filled rectangle.
    fn fill_rect(&mut self, paint: Paint, rect: Box2i);
}

/// A two-dimensional array of [`Paint`]s that can be efficiently drawn to compatible
/// [`ImageDrawer`]s.
pub struct Image<H: ImageHandle> {
    source: H,
    rect: <H::Source as ImageSource>::Rect,
}

/// Maintains a section of an [`ImageSource`] holding data for an [`Image`].
///
/// For atlas-based [`ImageSource`]s, holding an [`ImageHandle`] will prevent relevant data from
/// being overwritten.
pub trait ImageHandle {
    /// The type of backing storage used for this type of handle.
    ///
    /// This determines which [`ImageDrawer`]s can draw the image.
    type Source: ImageSource + ?Sized;

    /// Gets the underlying [`ImageSource`] for this handle.
    fn source(&self) -> &Self::Source;
}

impl<H: ImageHandle + ?Sized> ImageHandle for &H {
    type Source = H::Source;
    fn source(&self) -> &Self::Source {
        (**self).source()
    }
}

impl<H: ImageHandle + ?Sized> ImageHandle for Box<H> {
    type Source = H::Source;
    fn source(&self) -> &Self::Source {
        (**self).source()
    }
}

impl<H: ImageHandle + ?Sized> ImageHandle for Rc<H> {
    type Source = H::Source;
    fn source(&self) -> &Self::Source {
        (**self).source()
    }
}

impl<H: ImageHandle + ?Sized> ImageHandle for Arc<H> {
    type Source = H::Source;
    fn source(&self) -> &Self::Source {
        (**self).source()
    }
}

impl<H: ImageHandle> Image<H> {
    /// Constructs an [`Image`] from a handle and a location within the handle's [`ImageSource`].
    pub fn new(handle: H, rect: <H::Source as ImageSource>::Rect) -> Self {
        Image {
            source: handle,
            rect,
        }
    }

    /// The [`ImageSource`] where this image is stored.
    pub fn source(&self) -> &H::Source {
        self.source.source()
    }

    /// The location of this image within its [`ImageSource`].
    pub fn rect(&self) -> <H::Source as ImageSource>::Rect {
        self.rect
    }

    /// The size of this image.
    pub fn size(&self) -> Size2i {
        self.source.source().image_size(self.rect)
    }

    /// Gets an [`Image`] for a rectangular section of this image, or [`None`] if the requested
    /// section is out of bounds.
    pub fn view(self, sub: Box2i) -> Option<Image<H>>
    where
        Self: Sized,
    {
        let rect = self.source.source().image_part(self.rect, sub)?;
        Some(Image {
            source: self.source,
            rect,
        })
    }

    /// Converts this image to a view of the backing store.
    pub fn to_source(&self) -> Image<&H::Source> {
        Image {
            source: self.source.source(),
            rect: self.rect,
        }
    }

    /// Wraps the underlying handle in an [`Rc`], allowing the image to be cloned.
    pub fn into_rc(self) -> Image<Rc<H>> {
        Image {
            source: Rc::new(self.source),
            rect: self.rect,
        }
    }
}

impl<H: ImageHandle + Clone> Clone for Image<H> {
    fn clone(&self) -> Self {
        Image {
            source: self.source.clone(),
            rect: self.rect,
        }
    }
}

impl<H: ImageHandle + Copy> Copy for Image<H> {}

/// The backing storage for one or more images, for use with an [`ImageDrawer`].
pub trait ImageSource: ImageHandle<Source = Self> {
    /// Describes the location of an image within this store.
    type Rect: Eq + Copy;

    /// Gets the size of an image within this store.
    fn image_size(&self, rect: Self::Rect) -> Size2i;

    /// Gets the `Rect` for a section of an image within this store, or [`None`] if the
    /// requested section is out of bounds.
    fn image_part(&self, rect: Self::Rect, sub: Box2i) -> Option<Self::Rect>;
}

/// Provides functions for loading and preparing images.
pub trait ImageManager {
    /// The type of [`ImageSource`] used by this [`ImageManager`].
    type Source: ImageSource;

    /// The type of [`ImageHandle`] used to maintain a loaded image.
    type Handle: ImageHandle<Source = Self::Source>;

    /// Loads an `Image` from the given source data.
    fn load_image(&self, source: image::DynamicImage) -> Image<Self::Handle>;
}

/// An environment that has a preferred [`ImageManager`].
pub trait HasImageManager {
    /// The type of [`ImageManager`] for this environment.
    type ImageManager: ImageManager + Copy;

    /// Gets the [`ImageManager`] for this environment.
    fn image_manager(&self) -> Self::ImageManager;
}

/// A [`RasterDrawer`] which can draw images from a particular kind of [`ImageSource`].
pub trait ImageDrawer<Store: ImageSource + ?Sized>: RasterDrawer {
    /// Draws an image to the underlying surface. The color and opacity of the image will be
    /// multiplied by `paint`, and the image will be transformed by `trans` to position it
    /// on the surface.
    fn draw_image(&mut self, image: Image<&Store>, paint: Paint, trans: Ortho2i);
}

impl<T: ImageManager> ImageManager for &T {
    type Source = T::Source;
    type Handle = T::Handle;
    fn load_image(&self, source: image::DynamicImage) -> Image<T::Handle> {
        (**self).load_image(source)
    }
}

impl<T: VectorDrawer + ?Sized> VectorDrawer for &mut T {
    fn visible_bounds(&self) -> Box2 {
        (**self).visible_bounds()
    }

    fn draw_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcDrawer<'_> {
        (**self).draw_polyarc(paint, start)
    }

    type PolyarcDrawer<'a> = T::PolyarcDrawer<'a>
    where
        Self: 'a;

    fn fill_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcFiller<'_> {
        (**self).fill_polyarc(paint, start)
    }

    type PolyarcFiller<'a> = T::PolyarcFiller<'a>
    where
        Self: 'a;
}

impl<T: VectorDrawer> VectorDrawer for Transform<Similarity2, T> {
    fn visible_bounds(&self) -> Box2 {
        let inv_trans = self.transform.inverse();
        let source_bounds = self.source.visible_bounds();
        Box2::bound_points([
            inv_trans * vec2(source_bounds.min.x, source_bounds.min.y),
            inv_trans * vec2(source_bounds.max.x, source_bounds.min.y),
            inv_trans * vec2(source_bounds.min.x, source_bounds.max.y),
            inv_trans * vec2(source_bounds.max.x, source_bounds.max.y),
        ])
    }

    fn draw_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcDrawer<'_> {
        let start = self.transform * start;
        Transform::new(self.source.draw_polyarc(paint, start), self.transform)
    }

    type PolyarcDrawer<'a> = Transform<Similarity2, T::PolyarcDrawer<'a>>
    where
        Self: 'a;

    fn fill_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcFiller<'_> {
        let start = self.transform * start;
        Transform::new(self.source.fill_polyarc(paint, start), self.transform)
    }

    type PolyarcFiller<'a> = Transform<Similarity2, T::PolyarcFiller<'a>>
    where
        Self: 'a;
}

impl<T: RasterDrawer + ?Sized> RasterDrawer for &mut T {
    fn fill_rect(&mut self, paint: Paint, rect: Box2i) {
        (**self).fill_rect(paint, rect)
    }
}

impl<Store: ImageSource + ?Sized, T: ImageDrawer<Store>> ImageDrawer<Store> for &mut T {
    fn draw_image(&mut self, image: Image<&Store>, paint: Paint, trans: Ortho2i) {
        (**self).draw_image(image, paint, trans)
    }
}

/// Applies a transform of type `S` to an underlying drawing interface of type `T`.
pub struct Transform<S, T: ?Sized> {
    pub transform: S,
    pub source: T,
}

impl<S, T> Transform<S, T> {
    /// Constructs a transformed wrapper over the given object.
    pub fn new(source: T, transform: S) -> Self {
        Transform { transform, source }
    }
}
