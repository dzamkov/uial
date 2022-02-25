mod sizing;
mod transform;

pub use cgmath::vec2;
pub use cgmath::{Matrix2, Vector2};
pub use cgmath::ElementWise;
pub use cgmath::BaseNum;
pub use sizing::*;
pub use transform::*;
pub use GridDir2::{East, North, South, West};
pub use GridRotation2::{Ccw, Cw, Flip};

/// An axis-aligned box in two-dimensional space.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct Box2<S> {
    pub min: Vector2<S>,
    pub max: Vector2<S>,
}

impl<S: BaseNum> Box2<S> {
    /// Constructs a new [`Box2`] using the provided values.
    pub fn new(min_x: S, max_x: S, min_y: S, max_y: S) -> Self {
        Box2 {
            min: vec2(min_x, min_y),
            max: vec2(max_x, max_y),
        }
    }

    /// The size of this [`Box2`] along each axis.
    pub fn size(&self) -> Vector2<S> {
        self.max.sub_element_wise(self.min)
    }
}

impl Box2<i32> {
    /// Approximates this discrete box as a `f32` box.
    pub fn approx_into(self) -> Box2<f32> {
        Box2 {
            min: vec2(self.min.x as f32, self.min.y as f32),
            max: vec2(self.max.x as f32, self.max.y as f32),
        }
    }
}

/// Shortcut for [`Box2::new`].
pub fn box2<S: BaseNum>(min_x: S, max_x: S, min_y: S, max_y: S) -> Box2<S> {
    Box2::new(min_x, max_x, min_y, max_y)
}

impl<S: BaseNum> std::ops::Add<Vector2<S>> for Box2<S> {
    type Output = Box2<S>;

    fn add(self, rhs: Vector2<S>) -> Self::Output {
        Box2 {
            min: self.min + rhs,
            max: self.max + rhs,
        }
    }
}

/// Signed numeric types
pub trait BaseSignedNum: BaseNum + std::ops::Neg<Output = Self> {}

impl BaseSignedNum for i16 {}
impl BaseSignedNum for i32 {}

/// Identifies one of the 4 directions on a two-dimensional grid.
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum GridDir2 {
    /// The [`GridDir2`] that points in the positive X direction.
    East = 0,

    /// The [`GridDir2`] that points in the positive Y direction.
    North = 1,

    /// The [`GridDir2`] that points in the negative X direction.
    West = 2,

    /// The [`GridDir2`] that points in the negative Y direction.
    South = 3,
}

impl GridDir2 {
    /// An array of all possible [`GridDir2`]s.
    pub fn all() -> [Self; 4] {
        [East, North, West, South]
    }

    /// Gets a vector representation of this [`GridDir2`].
    pub fn to_vec<S: BaseSignedNum>(&self) -> Vector2<S> {
        let p = S::one();
        let z = S::zero();
        let n = -S::one();
        match self {
            East => vec2(p, z),
            North => vec2(z, p),
            West => vec2(n, z),
            South => vec2(z, n),
        }
    }
}

/// A spatially-translated wrapper over some underlying type or interface.
#[derive(Copy, Clone)]
pub struct Translate2<T: ?Sized, S: BaseSignedNum> {
    pub offset: Vector2<S>,
    pub source: T,
}

impl<T, S: BaseSignedNum> Translate2<T, S> {
    /// Constructs a translated wrapper over the given interface.
    pub fn new(source: T, offset: Vector2<S>) -> Self {
        Translate2 { offset, source }
    }

    /// Applies another translation to this [`Translate2`].
    pub fn translate(self, offset: Vector2<S>) -> Self {
        Translate2 {
            offset: offset + self.offset,
            source: self.source,
        }
    }
}

/// A spatially-transformed wrapper over some underlying type or interface.
pub struct GridMotate2<T: ?Sized, S> {
    pub trans: GridMotion2<S>,
    pub source: T,
}

impl<T, S> GridMotate2<T, S> {
    /// Constructs a translated wrapper over the given interface.
    pub fn new(source: T, trans: GridMotion2<S>) -> Self {
        GridMotate2 { trans, source }
    }
}

/// Stores a value of type `T` for each [`GridDir2`].
#[derive(Default)]
pub struct PerGridDir2<T>([T; 4]);

impl<T> std::ops::Index<GridDir2> for PerGridDir2<T> {
    type Output = T;
    fn index(&self, index: GridDir2) -> &Self::Output {
        unsafe { self.0.get_unchecked(index as u8 as usize) }
    }
}

impl<T> std::ops::IndexMut<GridDir2> for PerGridDir2<T> {
    fn index_mut(&mut self, index: GridDir2) -> &mut Self::Output {
        unsafe { self.0.get_unchecked_mut(index as u8 as usize) }
    }
}
