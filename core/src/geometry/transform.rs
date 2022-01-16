use crate::*;
use cgmath::{BaseFloat, Zero};

/// A coordinate transform in some space.
pub trait Transform {
    /// Gets the identity transform.
    fn identity() -> Self;

    /// Constructs a transform that applies `b`, followed by `a`.
    fn compose(a: Self, b: Self) -> Self;
}

/// A [`Transform`] that can be inverted.
pub trait InvertibleTransform {
    /// Constructs the inverse of this transform.
    fn inverse(&self) -> Self;
}

/// A transform in two-dimensional space.
pub trait Transform2<S>: Transform {
    /// Applies this transform to the given vector.
    fn transform(&self, vec: Vector2<S>) -> Vector2<S>;
}

// Implements the transform-related operators for a type.
macro_rules! impl_transform2_ops {
    ($ty:tt) => {
        impl std::ops::Mul<$ty> for $ty {
            type Output = $ty;
            fn mul(self, rhs: $ty) -> Self::Output {
                <$ty>::compose(self, rhs)
            }
        }
        impl<S: BaseSignedNum> std::ops::Mul<Vector2<S>> for $ty {
            type Output = Vector2<S>;
            fn mul(self, rhs: Vector2<S>) -> Self::Output {
                self.transform(rhs)
            }
        }
    };
    ($ident:ident < S : $trait:ident >) => {
        impl<S: $trait> std::ops::Mul<$ident<S>> for $ident<S> {
            type Output = $ident<S>;
            fn mul(self, rhs: $ident<S>) -> Self::Output {
                <$ident<S>>::compose(self, rhs)
            }
        }
        impl<S: $trait> std::ops::Mul<Vector2<S>> for $ident<S> {
            type Output = Vector2<S>;
            fn mul(self, rhs: Vector2<S>) -> Self::Output {
                self.transform(rhs)
            }
        }
    };
}

/// A rotational transform in two-dimensional space.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct Rotation2<S> {
    // Rotations are represented by the tangent of half their angle. This representation has several
    // benefits: it only uses one scalar value, it has roughly uniform precision over all rotations,
    // it does not require any expensive operations (e.g. sin, cos, sqrt) to apply or compose, and
    // there is no need to normalize after composing.
    tan_half_angle: S,
}

impl<S: BaseFloat> Rotation2<S> {
    /// Constructs a rotation which rotates counter-clockwise by the given angle, in radians.
    pub fn from_angle(angle: S) -> Self {
        let two = S::from(2.0).unwrap();
        Self {
            tan_half_angle: S::tan(angle / two),
        }
    }

    /// Computes the `sin` and `cos` of the angle for this rotation.
    pub fn angle_sin_cos(&self) -> (S, S) {
        let x = self.tan_half_angle;
        let one = S::one();
        let two = one + one;
        if S::abs(x) < Self::normal_threshold() {
            let x_sqr = x * x;
            let y = one / (one + x_sqr);
            (two * x * y, (one - x_sqr) * y)
        } else {
            (two / x, -one)
        }
    }

    /// The maximum absolute value of a `tan_half_angle` value can be handled using general logic.
    fn normal_threshold() -> S {
        S::from(1.0e18).unwrap() // TODO: Depends on type
    }
}

impl<S: BaseFloat> Transform for Rotation2<S> {
    fn identity() -> Self {
        Self {
            tan_half_angle: S::zero(),
        }
    }

    fn compose(a: Self, b: Self) -> Self {
        let x = a.tan_half_angle;
        let y = b.tan_half_angle;
        let normal_threshold = Self::normal_threshold();
        let one = S::one();
        if S::abs(x) <= normal_threshold {
            if S::abs(y) <= normal_threshold {
                Self {
                    tan_half_angle: (x + y) / (one - x * y),
                }
            } else {
                Self {
                    tan_half_angle: one / (one / y - x),
                }
            }
        } else {
            if S::abs(y) <= normal_threshold {
                Self {
                    tan_half_angle: one / (one / x - y),
                }
            } else {
                Self {
                    tan_half_angle: (-one / x) + (-one / y),
                }
            }
        }
    }
}

impl<S: BaseFloat> InvertibleTransform for Rotation2<S> {
    fn inverse(&self) -> Self {
        Rotation2 {
            tan_half_angle: -self.tan_half_angle,
        }
    }
}

impl<S: BaseFloat> Transform2<S> for Rotation2<S> {
    fn transform(&self, vec: Vector2<S>) -> Vector2<S> {
        Matrix2::from(*self) * vec
    }
}

impl_transform2_ops!(Rotation2<S: BaseFloat>);

impl<S: BaseFloat> From<Rotation2<S>> for Matrix2<S> {
    fn from(r: Rotation2<S>) -> Self {
        let (sin, cos) = r.angle_sin_cos();
        Matrix2::new(cos, sin, -sin, cos)
    }
}

#[test]
fn test_rot2() {
    let mut angle = 1.0f32;
    let mut rot = Rotation2::from_angle(angle);
    let delta_angle = 1.2f32;
    let delta_rot = Rotation2::from_angle(delta_angle);
    for _ in 0..100 {
        angle = angle + delta_angle;
        rot = rot * delta_rot;
        let vec = vec2(angle.cos(), angle.sin());
        let test_vec = rot * vec2(1.0, 0.0);
        cgmath::assert_abs_diff_eq!(vec, test_vec, epsilon = 0.001);
    }
}

/// A transformation in two-dimensional space consisting of rotation and translation. This is
/// also known as a "direct isometry".
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct Motion2<S> {
    pub rot: Rotation2<S>,
    pub offset: Vector2<S>,
}

impl<S: BaseFloat> Motion2<S> {
    /// Constructs a [`Motion2`] from a rotation and a translation.
    pub fn new(rot: Rotation2<S>, offset: Vector2<S>) -> Self {
        Self { rot, offset }
    }

    /// Constructs a [`Motion2`] from a translation.
    pub fn from_translation(offset: Vector2<S>) -> Self {
        Self {
            rot: Rotation2::identity(),
            offset,
        }
    }
}

impl<S: BaseFloat> Transform for Motion2<S> {
    fn identity() -> Self {
        Self {
            rot: Rotation2::identity(),
            offset: Vector2::zero(),
        }
    }

    fn compose(a: Self, b: Self) -> Self {
        Self {
            rot: Rotation2::compose(a.rot, b.rot),
            offset: a.transform(b.offset),
        }
    }
}

impl<S: BaseFloat> InvertibleTransform for Motion2<S> {
    fn inverse(&self) -> Self {
        todo!()
    }
}

impl<S: BaseFloat> Transform2<S> for Motion2<S> {
    fn transform(&self, vec: Vector2<S>) -> Vector2<S> {
        (self.rot * vec) + self.offset
    }
}

impl<S: BaseFloat> From<Rotation2<S>> for Motion2<S> {
    fn from(rot: Rotation2<S>) -> Self {
        Self {
            rot,
            offset: Vector2::zero(),
        }
    }
}

impl_transform2_ops!(Motion2<S: BaseFloat>);

/// A grid-aligned rotational transform in two-dimensional space.
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum GridRotation2 {
    /// The [`GridRotation2`] which does no rotation.
    Id = 0,

    /// The [`GridRotation2`] which rotates 90 degrees counter-clockwise.
    Ccw = 1,

    /// The [`GridRotation2`] which rotates 180 degrees.
    Flip = 2,

    /// The [`GridRotation2`] which rotates 90 degrees clockwise.
    Cw = 3,
}

impl GridRotation2 {
    /// Gets the unique rotation which, when applied to `a`, yields `b`.
    pub fn between(a: GridDir2, b: GridDir2) -> Self {
        let angle = (b as u8).wrapping_sub(a as u8) & 3;
        unsafe { std::mem::transmute(angle) }
    }

    /// Gets the discrete rotation that is closest to the given rotation.
    pub fn nearest<S: BaseFloat>(source: Rotation2<S>) -> Self {
        let two = S::from(2.0).unwrap();
        let sqrt_two = S::sqrt(two);
        let low_limit = (two - sqrt_two) / sqrt_two;
        let high_limit = S::one() + sqrt_two;
        let abs_tan_half_angle = S::abs(source.tan_half_angle);
        if abs_tan_half_angle < low_limit {
            GridRotation2::Id
        } else if abs_tan_half_angle > high_limit {
            GridRotation2::Flip
        } else if source.tan_half_angle > S::zero() {
            GridRotation2::Ccw
        } else {
            GridRotation2::Cw
        }
    }

    /// Determines the unique [`GridMotion2`] which applies this rotation to a [`Box2`] of the
    /// given dimensions while maintaining the minimum corner at the origin.
    pub fn on<S: BaseSignedNum>(self, size_x: S, size_y: S) -> GridMotion2<S> {
        GridMotion2::new(
            self,
            match self {
                GridRotation2::Id => vec2(S::zero(), S::zero()),
                GridRotation2::Ccw => vec2(size_y, S::zero()),
                GridRotation2::Flip => vec2(size_x, size_y),
                GridRotation2::Cw => vec2(S::zero(), size_x),
            },
        )
    }
}

impl Transform for GridRotation2 {
    fn identity() -> Self {
        GridRotation2::Id
    }

    fn compose(a: Self, b: Self) -> Self {
        let angle = ((a as u8) + (b as u8)) & 3;
        unsafe { std::mem::transmute(angle) }
    }
}

impl InvertibleTransform for GridRotation2 {
    fn inverse(&self) -> Self {
        let angle = (4 - *self as u8) & 3;
        unsafe { std::mem::transmute(angle) }
    }
}

impl<S: BaseSignedNum> Transform2<S> for GridRotation2 {
    fn transform(&self, vec: Vector2<S>) -> Vector2<S> {
        match self {
            GridRotation2::Id => vec,
            GridRotation2::Ccw => vec2(-vec.y, vec.x),
            GridRotation2::Flip => -vec,
            GridRotation2::Cw => vec2(vec.y, -vec.x),
        }
    }
}

impl<S: BaseFloat> From<GridRotation2> for Rotation2<S> {
    fn from(r: GridRotation2) -> Self {
        [
            Rotation2 {
                tan_half_angle: S::zero(),
            },
            Rotation2 {
                tan_half_angle: S::one(),
            },
            Rotation2 {
                tan_half_angle: S::infinity(),
            },
            Rotation2 {
                tan_half_angle: -S::one(),
            },
        ][r as usize]
    }
}

impl<S: BaseFloat> From<GridRotation2> for Motion2<S> {
    fn from(r: GridRotation2) -> Self {
        let r: Rotation2<S> = r.into();
        r.into()
    }
}

impl_transform2_ops!(GridRotation2);

#[test]
fn test_rot2_flip() {
    let rot: Rotation2<f32> = GridRotation2::Flip.into();
    cgmath::assert_abs_diff_eq!(vec2(-1.0, -2.0), rot * vec2(1.0, 2.0), epsilon = 0.001);
}

#[test]
fn test_rot2_nearest() {
    assert_eq!(GridRotation2::Id, GridRotation2::nearest(Rotation2::from_angle(0.2f32)));
    assert_eq!(GridRotation2::Ccw, GridRotation2::nearest(Rotation2::from_angle(1.2f32)));
    assert_eq!(GridRotation2::Flip, GridRotation2::nearest(Rotation2::from_angle(3f32)));
    assert_eq!(GridRotation2::Cw, GridRotation2::nearest(Rotation2::from_angle(5.2f32)));
}

/// A transformation in grid-aligned two-dimensional space, consisting of rotation and translation.
/// This is also known as a "direct isometry".
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct GridMotion2<S> {
    pub rot: GridRotation2,
    pub offset: Vector2<S>,
}

impl<S: BaseSignedNum> GridMotion2<S> {
    /// Constructs a [`GridMotion2`] from a rotation and a translation.
    pub fn new(rot: GridRotation2, offset: Vector2<S>) -> Self {
        Self { rot, offset }
    }

    /// Constructs a [`Motion2`] for the given translation.
    pub fn translation(offset: Vector2<S>) -> Self {
        Self {
            rot: GridRotation2::identity(),
            offset,
        }
    }

    /// Applies this motion to a box.
    pub fn transform_box(&self, b: Box2<S>) -> Box2<S> {
        match self.rot {
            GridRotation2::Id => Box2 {
                min: self.offset + b.min,
                max: self.offset + b.max,
            },
            GridRotation2::Ccw => Box2 {
                min: self.offset + vec2(-b.max.y, b.min.x),
                max: self.offset + vec2(-b.min.y, b.max.x),
            },
            GridRotation2::Flip => Box2 {
                min: self.offset - b.max,
                max: self.offset - b.min,
            },
            GridRotation2::Cw => Box2 {
                min: self.offset + vec2(b.min.y, -b.max.x),
                max: self.offset + vec2(b.max.y, -b.min.x),
            },
        }
    }
}

impl GridMotion2<i32> {
    /// Approximates this discrete grid motion as a [`Motion2`].
    pub fn approx_into(self) -> Motion2<f32> {
        Motion2 {
            rot: self.rot.into(),
            offset: vec2(self.offset.x as f32, self.offset.y as f32),
        }
    }
}

impl<S: BaseSignedNum> Transform for GridMotion2<S> {
    fn identity() -> Self {
        Self {
            rot: GridRotation2::identity(),
            offset: Vector2::zero(),
        }
    }

    fn compose(a: Self, b: Self) -> Self {
        Self {
            rot: GridRotation2::compose(a.rot, b.rot),
            offset: a.transform(b.offset),
        }
    }
}

impl<S: BaseSignedNum> InvertibleTransform for GridMotion2<S> {
    fn inverse(&self) -> Self {
        todo!()
    }
}

impl<S: BaseSignedNum> Transform2<S> for GridMotion2<S> {
    fn transform(&self, vec: Vector2<S>) -> Vector2<S> {
        (self.rot * vec) + self.offset
    }
}

impl From<GridMotion2<i16>> for GridMotion2<i32> {
    fn from(trans: GridMotion2<i16>) -> Self {
        Self {
            rot: trans.rot,
            offset: vec2(trans.offset.x.into(), trans.offset.y.into()),
        }
    }
}

impl_transform2_ops!(GridMotion2<S: BaseSignedNum>);

impl<S: BaseSignedNum> std::ops::Mul<Box2<S>> for GridMotion2<S> {
    type Output = Box2<S>;
    fn mul(self, rhs: Box2<S>) -> Self::Output {
        self.transform_box(rhs)
    }
}

impl<S: BaseSignedNum> std::ops::Mul<GridMotion2<S>> for GridRotation2 {
    type Output = GridMotion2<S>;
    fn mul(self, rhs: GridMotion2<S>) -> Self::Output {
        GridMotion2::new(self * rhs.rot, self * rhs.offset)
    }
}

/// An affine transformation in grid-aligned two-dimensional space, consisting of rotation,
/// non-uniform scaling, reflection and translation. Note that, due to requirement to remain
/// grid-aligned, skew transformations are not allowed.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct GridAffine2<S> {
    swap_x_y: bool,
    scale: Vector2<S>,
    pub offset: Vector2<S>,
}

impl<S: BaseSignedNum> GridAffine2<S> {
    /// Constructs a [`GridAffine2`] for the given translation.
    pub fn translation(offset: Vector2<S>) -> Self {
        Self {
            swap_x_y: false,
            scale: vec2(S::one(), S::one()),
            offset,
        }
    }

    /// Constructs a [`GridAffine2`] for the given non-uniform scaling.
    pub fn scaling(scale: Vector2<S>) -> Self {
        Self {
            swap_x_y: false,
            scale,
            offset: Vector2::zero()
        }
    }
}

impl<S: BaseSignedNum> Transform for GridAffine2<S> {
    fn identity() -> Self {
        Self {
            swap_x_y: false,
            scale: vec2(S::one(), S::one()),
            offset: Vector2::zero()
        }
    }

    fn compose(a: Self, b: Self) -> Self {
        let mut other_scale = b.scale;
        if a.swap_x_y {
            std::mem::swap(&mut other_scale.x, &mut other_scale.y);
        }
        Self {
            swap_x_y: a.swap_x_y ^ b.swap_x_y,
            scale: a.scale.mul_element_wise(other_scale),
            offset: a.transform(b.offset),
        }
    }
}

impl<S: BaseSignedNum> Transform2<S> for GridAffine2<S> {
    fn transform(&self, mut vec: Vector2<S>) -> Vector2<S> {
        if self.swap_x_y {
            std::mem::swap(&mut vec.x, &mut vec.y);
        }
        self.offset + self.scale.mul_element_wise(vec)
    }
}

impl_transform2_ops!(GridAffine2<S: BaseSignedNum>);