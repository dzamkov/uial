use crate::*;

/// A set of possible sizes for a two-dimensional graphical element, each associated with a
/// relative "cost". This can be used to describe the allowed sizes of a graphical element and
/// establish a preference hierarchy between them.
#[derive(fortify::Lower, Clone)]
pub struct Sizing {
    range: Box2<u32>,
}

impl Sizing {
    /// Constructs a [`Sizing`] which permits any size with equal preference.
    pub fn any() -> Self {
        Self {
            range: box2(0, u32::MAX, 0, u32::MAX),
        }
    }

    /// Constructs a [`Sizing`] which permits only the given size.
    pub fn exact(size: Vector2<u32>) -> Self {
        Self::in_range(box2(size.x, size.x + 1, size.y, size.y + 1))
    }

    /// Constructs a [`Sizing`] which only permits sizes within the given range.
    pub fn in_range(range: Box2<u32>) -> Self {
        Self { range }
    }

    /// Constructs a [`Sizing`] which permits exactly the sizes that are permitted by both of the
    /// given [`Sizing`]s. The cost of a size is the sum of its cost from both operands.
    pub fn intersect(a: &Self, b: &Self) -> Self {
        Self {
            range: Box2::intersect(&a.range, &b.range),
        }
    }

    /// Constructs a [`Sizing`] consisting of the sizes which can be produced by applying a given
    /// amount of padding to an element with the given [`Sizing`].
    pub fn with_padding(
        &self,
        width: u32,
        height: u32,
        stretch_width: bool,
        stretch_height: bool,
    ) -> Self {
        Self {
            range: box2(
                self.range.min.x + width,
                if stretch_width {
                    u32::MAX
                } else {
                    self.range.max.x.saturating_add(width)
                },
                self.range.min.y + height,
                if stretch_height {
                    u32::MAX
                } else {
                    self.range.max.y.saturating_add(height)
                },
            ),
        }
    }

    /// Determines how much the given size can be shrunk along the X axis without increasing its
    /// "cost" according to this [`Sizing`].
    pub fn stretch_width(&self, size: Vector2<u32>) -> u32 {
        size.x - self.range.min.x
    }

    /// Determines how much the given size can be shrunk along the Y axis without increasing its
    /// "cost" according to this [`Sizing`].
    pub fn stretch_height(&self, size: Vector2<u32>) -> u32 {
        size.y - self.range.min.y
    }
}
