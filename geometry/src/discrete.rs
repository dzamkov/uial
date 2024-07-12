use crate::Vector2;
use derive_more::{Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign};

/// A [`num_rational::Ratio`] of [`u32`]s.
pub type RationalU32 = num_rational::Ratio<u32>;

/// A vector in discrete two-dimensional space.
#[repr(C)]
#[derive(
    Default, PartialEq, Eq, Hash, Clone, Copy, Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign,
)]
pub struct Vector2i {
    pub x: i32,
    pub y: i32,
}

impl From<Vector2i> for [i32; 2] {
    #[inline]
    fn from(source: Vector2i) -> Self {
        [source.x, source.y]
    }
}

impl Vector2i {
    /// Converts this discrete vector into a continuous vector with the same component values.
    #[inline]
    pub fn into_float(self) -> Vector2 {
        Vector2 {
            x: self.x as f32,
            y: self.y as f32,
        }
    }
}

impl std::fmt::Debug for Vector2i {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("vec2i")
            .field(&self.x)
            .field(&self.y)
            .finish()
    }
}

/// Shortcut for constructing a [`Vector2i`].
#[inline(always)]
pub const fn vec2i(x: i32, y: i32) -> Vector2i {
    Vector2i { x, y }
}

/// Describes a point in discrete two-dimensional space.
pub type Point2i = Vector2i;

/// Describes a possible size of a rectangle in discrete two-dimensional space.
#[repr(C)]
#[derive(
    Debug, Default, PartialEq, Eq, Clone, Copy, Add, AddAssign, Mul, MulAssign, Sub, SubAssign,
)]
pub struct Size2i {
    pub x: u32,
    pub y: u32,
}

impl Size2i {
    /// Converts this [`Size2i`] into a [`Vector2i`] with the same component values.
    #[inline]
    pub fn into_vec(self) -> Vector2i {
        Vector2i {
            x: self.x as i32,
            y: self.y as i32,
        }
    }
}

impl From<Size2i> for [u32; 2] {
    #[inline]
    fn from(source: Size2i) -> Self {
        [source.x, source.y]
    }
}

/// Shortcut for constructing a [`Size2i`].
#[inline(always)]
pub const fn size2i(x: u32, y: u32) -> Size2i {
    Size2i { x, y }
}

impl PartialOrd for Size2i {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Size2i {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.y.cmp(&other.y) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        self.x.cmp(&other.x)
    }
}

/// Identifies an axis in two-dimensional space.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Axis2 {
    X,
    Y,
}

/// Identifies an orthogonal direction in two-dimensional space.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Dir2i {
    PosX,
    PosY,
    NegX,
    NegY,
}

/// Describes an axis-aligned rectangle in discrete two-dimensional space.
#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct Box2i {
    pub min: Point2i,
    pub max_exclusive: Point2i,
}

impl Box2i {
    /// Constructs a [`Box2i`] from the given minimum and (exclusive) maximum points.
    #[inline]
    pub fn from_min_max(min: Point2i, max: Point2i) -> Self {
        Self {
            min,
            max_exclusive: max,
        }
    }

    /// Constructs a [`Box2i`] from the given minimum point and size.
    #[inline]
    pub fn from_min_size(min: Point2i, size: Size2i) -> Self {
        Self {
            min,
            max_exclusive: min + size.into_vec(),
        }
    }

    /// Determines whether this box contains the given point.
    ///
    /// This returns `false` for points on the box's maximum boundary lines.
    #[inline]
    pub fn contains_exclusive(&self, point: Point2i) -> bool {
        self.min.x <= point.x
            && self.min.y <= point.y
            && point.x < self.max_exclusive.x
            && point.y < self.max_exclusive.y
    }

    /// Determines whether this box has any points in common with the given box.
    #[inline]
    pub fn overlaps(&self, other: Box2i) -> bool {
        self.min.x < other.max_exclusive.x
            && self.min.y < other.max_exclusive.y
            && other.min.x < self.max_exclusive.x
            && other.min.y < self.max_exclusive.y
    }

    /// Gets the size of this box.
    #[inline]
    pub fn size(&self) -> Size2i {
        size2i(
            (self.max_exclusive.x - self.min.x) as u32,
            (self.max_exclusive.y - self.min.y) as u32,
        )
    }
}

/// Describes a directionally-varying amount of padding that can be applied to a [`Rect2i`].
#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct Padding2i {
    pub n_x: u32,
    pub n_y: u32,
    pub p_x: u32,
    pub p_y: u32,
}

impl Padding2i {
    /// Constructs uniform padding of the given amount.
    #[inline]
    pub fn uniform(amount: u32) -> Self {
        Self {
            n_x: amount,
            n_y: amount,
            p_x: amount,
            p_y: amount,
        }
    }

    /// Gets the total amount of size added by this padding.
    #[inline]
    pub fn size(&self) -> Size2i {
        Size2i {
            x: self.n_x + self.p_x,
            y: self.n_y + self.p_y,
        }
    }
}

/// A transform in discrete two-dimensional space consisting of rotation, translation, reflection
/// and uniform scaling.
#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct Similarity2i {
    // TODO: Rotation and scaling
    /// The offset for the translation component of this transform, applied after rotation and
    /// scaling.
    pub offset: Vector2i,
}

impl Similarity2i {
    /// Constructs a [`Similarity2i`] which translates by the given offset.
    #[inline]
    pub const fn translate(offset: Vector2i) -> Self {
        Self { offset }
    }
}

impl std::ops::Mul<Vector2i> for Similarity2i {
    type Output = Vector2i;
    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, point: Vector2i) -> Vector2i {
        point + self.offset
    }
}
