mod text;
mod polyline;

use super::*;
use uial_geometry::{Similarity2, Similarity2i};
pub use polyline::*;
pub use text::*;

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

/// The backing storage for one or more images, for use with a [`RasterDrawer`].
pub trait ImageStore {
    /// Describes the location of an image within this store.
    type ImageRect: Copy;

    /// Gets the size of the given `ImageRect`.
    fn image_rect_size(&self, rect: Self::ImageRect) -> Size2i;

    /// Gets a rectangular sub-section of the given `ImageRect`, or returns [`None`] if the section
    /// is out of bounds.
    fn image_rect_part(&self, rect: Self::ImageRect, sub: Box2i) -> Option<Self::ImageRect>;
}

/// A reference to a rectangular section of an image, i.e. a two-dimensional grid of [`Paint`]s.
pub struct ImageView<'a, Store: 'a + ImageStore + ?Sized> {
    store: &'a Store,
    rect: Store::ImageRect,
}

impl<'a, Store: ImageStore + ?Sized> ImageView<'a, Store> {
    /// Constructs an [`ImageView`] from the given data.
    pub fn new(store: &'a Store, rect: Store::ImageRect) -> Self {
        Self { store, rect }
    }

    /// Gets the [`ImageStore`] where this image is stored.
    pub fn store(&self) -> &'a Store {
        self.store
    }

    /// Gets the location of this image within its [`ImageStore`].
    pub fn rect(&self) -> Store::ImageRect {
        self.rect
    }
}

/// A type that can be interpreted as an [`ImageView`].
pub trait AsImageView<Store: ImageStore + ?Sized> {
    /// The size of this image.
    fn size(&self) -> Size2i {
        let view = self.view_all();
        view.store.image_rect_size(view.rect)
    }

    /// Gets a rectangular sub-section of this image, or returns [`None`] if the rectangle is out
    /// of bounds.
    fn view(&self, rect: Box2i) -> Option<ImageView<Store>> {
        let view = self.view_all();
        view.store
            .image_rect_part(view.rect, rect)
            .map(|sub| ImageView::new(view.store, sub))
    }

    /// Gets an [`ImageView`] for this entire image.
    fn view_all(&self) -> ImageView<Store>;
}

impl<Store: ImageStore + ?Sized> AsImageView<Store> for ImageView<'_, Store> {
    fn size(&self) -> Size2i {
        self.store.image_rect_size(self.rect)
    }

    fn view(&self, rect: Box2i) -> Option<ImageView<Store>> {
        self.store
            .image_rect_part(self.rect, rect)
            .map(|sub| ImageView::new(self.store, sub))
    }

    fn view_all(&self) -> ImageView<Store> {
        ImageView {
            store: self.store,
            rect: self.rect,
        }
    }
}

/// Provides functions for loading and preparing images.
pub trait ImageManager {
    /// The type of [`ImageStore`] used by this [`ImageManager`].
    type Store: ImageStore;

    /// A two-dimensional grid of [`Paint`]s
    type Image: AsImageView<Self::Store>;

    /// Loads an `Image` from the given source data.
    fn load_image(&self, source: image::DynamicImage) -> Self::Image;
}

/// An environment that has a preferred [`ImageManager`].
pub trait HasImageManager {
    /// The type of [`ImageManager`] for this environment.
    type ImageManager: ImageManager + Copy;

    /// Gets the [`ImageManager`] for this environment.
    fn image_manager(&self) -> Self::ImageManager;
}

/// A [`RasterDrawer`] which can draw images from a particular kind of [`ImageStore`].
pub trait ImageDrawer<Store: ImageStore + ?Sized>: RasterDrawer {
    /// Draws an image to the underlying surface. The color and opacity of the image will be
    /// multiplied by `paint`, and the image will be transformed by `trans` to position it
    /// on the surface.
    fn draw_image(&mut self, image: ImageView<Store>, paint: Paint, trans: Similarity2i);
}

impl<T: ImageManager> ImageManager for &T {
    type Store = T::Store;
    type Image = T::Image;
    fn load_image(&self, source: image::DynamicImage) -> Self::Image {
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

impl<Store: ImageStore + ?Sized, T: ImageDrawer<Store>> ImageDrawer<Store> for &mut T {
    fn draw_image(&mut self, image: ImageView<Store>, paint: Paint, trans: Similarity2i) {
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