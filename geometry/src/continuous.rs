use derive_more::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

/// The scalar type used for continuous geometry.
pub type Scalar = f32;

/// The [`Scalar`] value of π.
pub const PI: Scalar = std::f32::consts::PI;

/// A vector in continuous two-dimensional space.
#[repr(C)]
#[derive(
    Default,
    PartialEq,
    Clone,
    Copy,
    Add,
    AddAssign,
    Mul,
    MulAssign,
    Neg,
    Sub,
    SubAssign,
    Div,
    DivAssign,
)]
pub struct Vector2 {
    pub x: Scalar,
    pub y: Scalar,
}

impl Vector2 {
    /// Constructs a new vector from its components.
    #[inline]
    pub const fn new(x: Scalar, y: Scalar) -> Self {
        Self { x, y }
    }

    /// Gets a vector that is the same length and perpendicular to this vector, equivalent to
    /// rotating the vector 90 degrees counter-clockwise.
    #[inline]
    pub fn cross(&self) -> Vector2 {
        vec2(-self.y, self.x)
    }

    /// Computes the dot product of the given vectors.
    #[inline]
    pub fn dot(&self, b: &Vector2) -> Scalar {
        self.x * b.x + self.y * b.y
    }

    /// Gets the square of the L2 norm of this vector.
    #[inline]
    pub fn norm_squared(&self) -> Scalar {
        self.dot(self)
    }

    /// Gets the L2 norm of this vector.
    #[inline]
    pub fn norm(&self) -> Scalar {
        self.norm_squared().sqrt()
    }

    /// The direction of this vector.
    #[inline]
    pub fn normalize(&self) -> Vector2 {
        *self * (1.0 / self.norm())
    }

    /// Gets the signed angle, in radians, between this vector and another, in the range (-π, π].
    /// This will be positive if `other` is to the left (counter-clockwise) from this vector.
    pub fn angle_between(&self, other: &Vector2) -> Scalar {
        let x = self.dot(other);
        let y = self.cross().dot(other);
        y.atan2(x)
    }
}

/// Shortcut for constructing a vector from its components.
#[inline(always)]
pub const fn vec2(x: Scalar, y: Scalar) -> Vector2 {
    Vector2::new(x, y)
}

impl From<[Scalar; 2]> for Vector2 {
    fn from([x, y]: [Scalar; 2]) -> Vector2 {
        vec2(x, y)
    }
}

impl From<Vector2> for [Scalar; 2] {
    fn from(v: Vector2) -> [Scalar; 2] {
        [v.x, v.y]
    }
}

impl std::fmt::Debug for Vector2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("vec2").field(&self.x).field(&self.y).finish()
    }
}

impl approx::AbsDiffEq for Vector2 {
    type Epsilon = <Scalar as approx::AbsDiffEq>::Epsilon;
    fn default_epsilon() -> Self::Epsilon {
        Scalar::default_epsilon()
    }

    fn abs_diff_eq(&self, other: &Self, epsilon: Self::Epsilon) -> bool {
        self.x.abs_diff_eq(&other.x, epsilon) && self.y.abs_diff_eq(&other.y, epsilon)
    }
}

impl approx::RelativeEq for Vector2 {
    fn default_max_relative() -> Self::Epsilon {
        Scalar::default_max_relative()
    }

    fn relative_eq(
        &self,
        other: &Self,
        epsilon: Self::Epsilon,
        max_relative: Self::Epsilon,
    ) -> bool {
        self.x.relative_eq(&other.x, epsilon, max_relative)
            && self.y.relative_eq(&other.y, epsilon, max_relative)
    }
}

/// Describes an axis-aligned rectangle in two-dimensional space.
#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Box2 {
    pub min: Vector2,
    pub max: Vector2,
}

impl Box2 {
    /// Constructs a [`Box2`] which covers all of two-dimensional space.
    pub const fn all() -> Self {
        Self {
            min: vec2(Scalar::NEG_INFINITY, Scalar::NEG_INFINITY),
            max: vec2(Scalar::INFINITY, Scalar::INFINITY),
        }
    }

    /// Constructs a [`Box2`] which contains no points.
    pub const fn none() -> Self {
        Self {
            min: vec2(Scalar::INFINITY, Scalar::INFINITY),
            max: vec2(Scalar::NEG_INFINITY, Scalar::NEG_INFINITY),
        }
    }

    /// Constructs the smallest [`Box2`] which contains all of the given points.
    pub fn bound_points(points: impl IntoIterator<Item = Vector2>) -> Self {
        let mut min = vec2(Scalar::INFINITY, Scalar::INFINITY);
        let mut max = vec2(Scalar::NEG_INFINITY, Scalar::NEG_INFINITY);
        for p in points {
            min.x = min.x.min(p.x);
            min.y = min.y.min(p.y);
            max.x = max.x.max(p.x);
            max.y = max.y.max(p.y);
        }
        Self { min, max }
    }

    /// Determines whether this box contains the given point.
    pub fn contains(&self, point: Vector2) -> bool {
        self.min.x <= point.x
            && point.x < self.max.x
            && self.min.y <= point.y
            && point.y < self.max.y
    }

    /// Constructs the smallest [`Box2`] which contains both of the given boxes.
    pub fn combine(&self, other: &Self) -> Self {
        Self {
            min: vec2(self.min.x.min(other.min.x), self.min.y.min(other.min.y)),
            max: vec2(self.max.x.max(other.max.x), self.max.y.max(other.max.y)),
        }
    }

    /// Constructs the smallest [`Box2`] which contains this box and the given point.
    pub fn combine_point(&self, point: Vector2) -> Self {
        Self {
            min: vec2(self.min.x.min(point.x), self.min.y.min(point.y)),
            max: vec2(self.max.x.max(point.x), self.max.y.max(point.y)),
        }
    }

    /// Determines whether this box has any points in common with the given box.
    pub fn overlaps(&self, other: &Self) -> bool {
        self.min.x < other.max.x
            && self.min.y < other.max.y
            && other.min.x < self.max.x
            && other.min.y < self.max.y
    }
}

/// A 2x2 matrix, describing a linear transformation in two-dimensional space.
#[derive(
    PartialEq,
    Copy,
    Clone,
    Debug,
    Add,
    AddAssign,
    Mul,
    MulAssign,
    Neg,
    Sub,
    SubAssign,
    Div,
    DivAssign,
)]
pub struct Matrix2 {
    pub x: Vector2,
    pub y: Vector2,
}

impl Matrix2 {
    /// Gets the identity matrix.
    #[inline]
    pub const fn identity() -> Self {
        Self {
            x: vec2(1.0, 0.0),
            y: vec2(0.0, 1.0),
        }
    }

    /// Gets the inverse of this matrix.
    #[inline]
    pub fn inverse(&self) -> Self {
        let det = self.x.x * self.y.y - self.x.y * self.y.x;
        Self {
            x: vec2(self.y.y, -self.x.y) / det,
            y: vec2(-self.y.x, self.x.x) / det,
        }
    }
}

impl std::ops::Mul<Vector2> for Matrix2 {
    type Output = Vector2;
    fn mul(self, rhs: Vector2) -> Vector2 {
        self.x * rhs.x + self.y * rhs.y
    }
}

impl std::ops::Mul<Matrix2> for Matrix2 {
    type Output = Matrix2;
    fn mul(self, rhs: Matrix2) -> Matrix2 {
        Matrix2 {
            x: self * rhs.x,
            y: self * rhs.y,
        }
    }
}

/// A rotational transform in two-dimensional space.
#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Rotation2 {
    /// The tangent of half the angle of the rotation (positive values correspond to
    /// counter-clockwise rotations).
    ///
    /// This representation has several advantages over more popular rotation representations:
    ///  * It requires only one scalar value.
    ///  * Unlike angle or complex number representations, it does not require normalization to
    ///    to preserve precision/accuracy after composing many rotations.
    ///  * No need to evaluate transcendental functions to apply or compose rotations.
    ///  * Precision is fairly uniform across the entire range of possible rotations.
    tan_half_angle: Scalar,
}

impl Rotation2 {
    /// The maximum absolute value for a `tan_half_angle` value that can be handled using general
    /// logic.
    const NORMAL_THRESHOLD: Scalar = 1e18;

    /// The identity rotation.
    pub const IDENTITY: Rotation2 = Rotation2 {
        tan_half_angle: 0.0,
    };

    /// A rotation which rotates counter-clockwise by 90 degrees.
    pub const CCW_90: Rotation2 = Rotation2 {
        tan_half_angle: 1.0,
    };

    /// A rotation which rotates clockwise by 90 degrees.
    pub const CW_90: Rotation2 = Rotation2 {
        tan_half_angle: -1.0,
    };

    /// A rotation which rotates by 180 degrees.
    pub const FLIP: Rotation2 = Rotation2 {
        tan_half_angle: Scalar::INFINITY,
    };

    /// Constructs a rotation which rotates counter-clockwise by the given angle, in radians.
    pub fn from_angle(angle: Scalar) -> Self {
        Self {
            tan_half_angle: (angle / 2.0).tan(),
        }
    }

    /// Constructs a rotation which rotates `vec2(1.0, 0.0)` to the given target direction.
    pub fn from_dir(dir: Vector2) -> Self {
        Self::from_angle(Vector2::angle_between(&vec2(1.0, 0.0), &dir))
    }

    /// Gets the "inverse" of this rotation, which rotates by the same amount in the opposite
    /// direction.
    pub fn inverse(&self) -> Self {
        Self {
            tan_half_angle: -self.tan_half_angle,
        }
    }

    /// Computes the `sin` and `cos` of the angle for this rotation.
    pub fn angle_sin_cos(&self) -> (Scalar, Scalar) {
        let x = self.tan_half_angle;
        if x.abs() < Self::NORMAL_THRESHOLD {
            let x_sqr = x * x;
            let y = 1.0 / (1.0 + x_sqr);
            (2.0 * x * y, (1.0 - x_sqr) * y)
        } else {
            (2.0 / x, -1.0)
        }
    }
}

impl std::ops::Mul<Rotation2> for Rotation2 {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        let x = self.tan_half_angle;
        let y = rhs.tan_half_angle;
        if x.abs() <= Self::NORMAL_THRESHOLD {
            if y.abs() <= Self::NORMAL_THRESHOLD {
                Self {
                    tan_half_angle: (x + y) / (1.0 - x * y),
                }
            } else {
                Self {
                    tan_half_angle: 1.0 / (1.0 / y - x),
                }
            }
        } else if y.abs() <= Self::NORMAL_THRESHOLD {
            Self {
                tan_half_angle: 1.0 / (1.0 / x - y),
            }
        } else {
            Self {
                tan_half_angle: (-1.0 / x) + (-1.0 / y),
            }
        }
    }
}

impl std::ops::Mul<Vector2> for Rotation2 {
    type Output = Vector2;
    fn mul(self, rhs: Vector2) -> Vector2 {
        let (sin, cos) = self.angle_sin_cos();
        vec2(cos * rhs.x - sin * rhs.y, sin * rhs.x + cos * rhs.y)
    }
}

impl From<Rotation2> for Matrix2 {
    fn from(rotation: Rotation2) -> Matrix2 {
        let (sin, cos) = rotation.angle_sin_cos();
        Matrix2 {
            x: vec2(cos, sin),
            y: vec2(-sin, cos),
        }
    }
}

#[test]
fn test_rotation_compose() {
    let mut angle = 1.0f32;
    let mut rot = Rotation2::from_angle(angle);
    let delta_angle = 1.2f32;
    let delta_rot = Rotation2::from_angle(delta_angle);
    for _ in 0..100 {
        angle += delta_angle;
        rot = rot * delta_rot;
        let vec = vec2(angle.cos(), angle.sin());
        let test_vec = rot * vec2(1.0, 0.0);
        approx::assert_relative_eq!(vec, test_vec, epsilon = 0.001);
    }
}

#[test]
fn test_rotation_consts() {
    let vec = vec2(1.0, 0.2);
    approx::assert_relative_eq!(Rotation2::CCW_90 * vec, vec2(-0.2, 1.0));
    approx::assert_relative_eq!(Rotation2::CW_90 * vec, vec2(0.2, -1.0));
    approx::assert_relative_eq!(Rotation2::FLIP * vec, vec2(-1.0, -0.2));
}

/// A transform in two-dimensional space consisting of rotation and translation.
#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Motion2 {
    /// The rotation component of this transform, applied before translation.
    pub rotation: Rotation2,

    /// The offset for the translation component of this transform, applied after rotation.
    pub offset: Vector2,
}

impl Motion2 {
    /// The identity motion.
    pub const IDENTITY: Self = Self {
        rotation: Rotation2::IDENTITY,
        offset: vec2(0.0, 0.0),
    };

    /// Constructs a motion which translates by the given offset.
    pub const fn translate(offset: Vector2) -> Self {
        Self {
            rotation: Rotation2::IDENTITY,
            offset,
        }
    }

    /// Gets the linear component of this motion.
    pub const fn linear(&self) -> Rotation2 {
        self.rotation
    }
}

impl std::ops::Mul<Motion2> for Motion2 {
    type Output = Motion2;
    fn mul(self, rhs: Motion2) -> Motion2 {
        Motion2 {
            rotation: self.rotation * rhs.rotation,
            offset: self.rotation * rhs.offset + self.offset,
        }
    }
}

impl std::ops::Mul<Vector2> for Motion2 {
    type Output = Vector2;
    fn mul(self, rhs: Vector2) -> Vector2 {
        self.rotation * rhs + self.offset
    }
}

/// A transform in two-dimensional space consisting of rotation, translation, reflection and
/// uniform scaling.
#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Similarity2 {
    /// The rotation component of this transform, applied before translation.
    pub rotation: Rotation2,

    /// The scaling/reflection component of this transform, applied before translation.
    pub scaling: Scalar,

    /// The offset for the translation component of this transform, applied after rotation and
    /// scaling.
    pub offset: Vector2,
}

impl Similarity2 {
    /// The identity similarity.
    pub const IDENTITY: Self = Self {
        rotation: Rotation2::IDENTITY,
        scaling: 1.0,
        offset: vec2(0.0, 0.0),
    };

    /// Constructs a similarity which translates by the given offset.
    pub const fn translate(offset: Vector2) -> Self {
        Self {
            rotation: Rotation2::IDENTITY,
            scaling: 1.0,
            offset,
        }
    }

    /// Constructs a similarity which scales by the given factor.
    pub const fn scale(scaling: Scalar) -> Self {
        Self {
            rotation: Rotation2::IDENTITY,
            scaling,
            offset: vec2(0.0, 0.0),
        }
    }

    /// Gets the inverse of this similarity.
    pub fn inverse(&self) -> Self {
        let rotation = self.rotation.inverse();
        let scaling = 1.0 / self.scaling;
        Self {
            rotation,
            scaling,
            offset: rotation * (-self.offset * scaling),
        }
    }

    /// Gets the linear component (consisting of rotation and scaling) for this similarity.
    pub fn linear(&self) -> Matrix2 {
        Matrix2::from(self.rotation) * self.scaling
    }
}

impl From<Rotation2> for Similarity2 {
    fn from(rotation: Rotation2) -> Similarity2 {
        Similarity2 {
            rotation,
            scaling: 1.0,
            offset: vec2(0.0, 0.0),
        }
    }
}

impl std::ops::Mul<Similarity2> for Similarity2 {
    type Output = Similarity2;
    fn mul(self, rhs: Similarity2) -> Similarity2 {
        Similarity2 {
            rotation: self.rotation * rhs.rotation,
            scaling: self.scaling * rhs.scaling,
            offset: self.rotation * (rhs.offset * self.scaling) + self.offset,
        }
    }
}

impl std::ops::Mul<Vector2> for Similarity2 {
    type Output = Vector2;
    fn mul(self, rhs: Vector2) -> Vector2 {
        self.rotation * (rhs * self.scaling) + self.offset
    }
}

/// An affine transform in two-dimensional space.
#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Affine2 {
    /// The linear component of this transform, applied before translation.
    pub linear: Matrix2,

    /// The offset for the translation component of this transform, applied after the linear
    /// component.
    pub offset: Vector2,
}

impl Affine2 {
    /// The identity transform.
    pub const IDENTITY: Self = Self {
        linear: Matrix2::identity(),
        offset: vec2(0.0, 0.0),
    };

    /// Constructs an affine transform which translates by the given offset.
    pub fn translate(offset: Vector2) -> Self {
        Self {
            linear: Matrix2::identity(),
            offset,
        }
    }

    /// Constructs an affine transform which scales non-uniformly by the given factors.
    pub fn scale(x: Scalar, y: Scalar) -> Self {
        Self {
            linear: Matrix2 {
                x: vec2(x, 0.0),
                y: vec2(0.0, y),
            },
            offset: vec2(0.0, 0.0),
        }
    }

    /// Gets the inverse of this affine transform.
    pub fn inverse(&self) -> Self {
        let linear = self.linear.inverse();
        Self {
            linear,
            offset: linear * (-self.offset),
        }
    }
}

impl From<Rotation2> for Affine2 {
    fn from(rotation: Rotation2) -> Affine2 {
        Affine2 {
            linear: Matrix2::from(rotation),
            offset: vec2(0.0, 0.0),
        }
    }
}

impl From<Similarity2> for Affine2 {
    fn from(similarity: Similarity2) -> Affine2 {
        Affine2 {
            linear: similarity.linear(),
            offset: similarity.offset,
        }
    }
}

impl std::ops::Mul<Affine2> for Affine2 {
    type Output = Affine2;
    fn mul(self, rhs: Affine2) -> Affine2 {
        Affine2 {
            linear: self.linear * rhs.linear,
            offset: self.linear * rhs.offset + self.offset,
        }
    }
}

impl std::ops::Mul<Similarity2> for Affine2 {
    type Output = Affine2;
    fn mul(self, rhs: Similarity2) -> Affine2 {
        self * Affine2::from(rhs)
    }
}

impl std::ops::Mul<Rotation2> for Affine2 {
    type Output = Affine2;
    fn mul(self, rhs: Rotation2) -> Affine2 {
        self * Affine2::from(rhs)
    }
}

impl std::ops::Mul<Vector2> for Affine2 {
    type Output = Vector2;
    fn mul(self, rhs: Vector2) -> Vector2 {
        self.linear * rhs + self.offset
    }
}

#[test]
fn test_affine_compose() {
    let a = Affine2::from(Similarity2 {
        rotation: Rotation2::from_angle(1.0),
        scaling: 2.0,
        offset: vec2(1.0, 2.0),
    });
    let b = Affine2::from(Similarity2 {
        rotation: Rotation2::from_angle(0.5),
        scaling: 0.5,
        offset: vec2(2.0, 1.0),
    });
    let c = Affine2::from(Similarity2 {
        rotation: Rotation2::from_angle(1.5),
        scaling: 1.0,
        offset: vec2(3.0, 3.0),
    });
    let x = vec2(5.0, 7.0);
    approx::assert_relative_eq!(a * (b * (c * x)), ((a * b) * c) * x, epsilon = 0.001);
    approx::assert_relative_eq!(a * (b * (c * x)), (a * (b * c)) * x, epsilon = 0.001);
}
