use crate::*;

/// An interface for a graphics context.
pub trait Graphics {
    /// An image that can be loaded and drawn using this graphics context.
    type Image;

    /// An interface for drawing to a two-dimensional surface using this graphics context.
    type Drawer<'a>: Drawer<Image = Self::Image>;

    /// Loads an image for use with the graphics context.
    // TODO: Also accept references to image data.
    fn load_image(&self, data: image::DynamicImage) -> Self::Image;
}

/// An interface for precisely drawing on a discrete two-dimensional surface.
pub trait Drawer {
    /// An image that can be loaded and drawn using this drawer.
    type Image;

    /// Loads an image for use with this drawer.
    // TODO: Also accept references to image data.
    fn load_image(&mut self, data: image::DynamicImage) -> Self::Image;

    /// Draws a section of an image to the target surface. To determine its final placement on the
    /// surface, the section will be translated so that its bottom left is at the origin, and
    /// then it will be transformed by `trans`. If the image contains alpha information, it will
    /// be blended with existing surface contents using standard compositing.
    fn draw_image(
        &mut self,
        image: &Self::Image,
        paint: Paint,
        src: Box2<i32>,
        trans: GridAffine2<i32>,
    );

    /// Fills a rectangular area on the target surface with a particular [`Paint`].
    fn fill_rect(&mut self, paint: Paint, loc: Box2<i32>) {
        let mut white_pixel = image::RgbImage::new(1, 1);
        white_pixel[(0, 0)].0 = [255, 255, 255];
        let white_pixel = self.load_image(image::DynamicImage::ImageRgb8(white_pixel));
        self.draw_image(
            &white_pixel,
            paint,
            box2(0, 1, 0, 1),
            GridAffine2::translation(loc.min) * GridAffine2::scaling(loc.size()),
        );
    }
}

/// Identifies an opaque color.
#[derive(Copy, Clone)]
pub struct Color([u8; 3]);

impl Color {
    /// Constructs a color from the given sRGB components.
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        Self([r, g, b])
    }

    /// Gets the red component of this [`Color`].
    pub const fn r(&self) -> u8 {
        self.0[0]
    }

    /// Gets the green component of this [`Color`].
    pub const fn g(&self) -> u8 {
        self.0[1]
    }

    /// Gets the blue component of this [`Color`].
    pub const fn b(&self) -> u8 {
        self.0[2]
    }
}

/// A [`Color`] with a transparency channel.
#[derive(Copy, Clone)]
pub struct Paint([u8; 4]);

impl Paint {
    /// Constructs a paint from the given sRGBA components.
    pub const fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self([r, g, b, a])
    }

    /// Gets the red component of this [`Paint`].
    pub const fn r(&self) -> u8 {
        self.0[0]
    }

    /// Gets the green component of this [`Paint`].
    pub const fn g(&self) -> u8 {
        self.0[1]
    }

    /// Gets the blue component of this [`Paint`].
    pub const fn b(&self) -> u8 {
        self.0[2]
    }

    /// Gets the alpha component of this [`Paint`].
    pub const fn a(&self) -> u8 {
        self.0[3]
    }
}

impl From<Color> for Paint {
    fn from(color: Color) -> Self {
        Paint::new(color.r(), color.g(), color.b(), 255)
    }
}

impl<'a, T: Graphics + ?Sized> Graphics for &'a T {
    type Image = T::Image;
    type Drawer<'b> = T::Drawer<'b>;

    fn load_image(&self, data: image::DynamicImage) -> Self::Image {
        (*self).load_image(data)
    }
}

impl<'a, T: Drawer + ?Sized> Drawer for &'a mut T {
    type Image = T::Image;

    fn load_image(&mut self, data: image::DynamicImage) -> Self::Image {
        (*self).load_image(data)
    }

    fn draw_image(
        &mut self,
        image: &Self::Image,
        paint: Paint,
        src: Box2<i32>,
        trans: GridAffine2<i32>,
    ) {
        (*self).draw_image(image, paint, src, trans)
    }
}
