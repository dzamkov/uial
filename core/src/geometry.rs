pub use diffgeom::shape::{size2i, Box2, Box2i, Size2i};
pub use diffgeom::time::{Duration, Instant};
pub use diffgeom::{vec2, Scalar, Vector2, PI};
pub use diffgeom::{vec2i, Dir2i, Vector2i};
pub use diffgeom::{Motion2, Rotation2, Similarity2};
pub use diffgeom::{Motion2i, Ortho2i, Rotation2i};

/// Describes a point in discrete two-dimensional space.
pub type Point2i = Vector2i;

/// Describes a directionally-varying amount of padding that can be applied to a [`Box2i`].
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
}