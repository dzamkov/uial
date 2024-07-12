use super::Scalar;
use std::ops::{Add, AddAssign, Div, DivAssign, Sub, SubAssign};

/// Represents a span of time using a fixed-point representation that ensures consistent precision
/// across all range of values. This is similar to [`core::time::Duration`], but more compact
/// and with a smaller range.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Duration {
    nanos: u64,
}

const NANOS_PER_SEC: u64 = 1_000_000_000;
const NANOS_PER_MILLI: u64 = 1_000_000;

/// Identifies a point in time using a [`Duration`] from a reference point (i.e. zero).
pub type Instant = Duration;

impl Duration {
    /// The zero-length [`Duration`].
    pub const ZERO: Duration = Duration { nanos: 0 };

    /// The largest representable [`Duration`].
    pub const MAX: Duration = Duration { nanos: u64::MAX };

    /// The smallest non-zero [`Duration`].
    pub const MIN_POSITIVE: Duration = Duration { nanos: 1 };

    /// Constructs a [`Duration`] for the given number of whole seconds. Panics on overflow.
    pub fn from_secs(secs: u64) -> Duration {
        Duration::try_from_secs(secs).expect("duration overflow")
    }

    /// Constructs a [`Duration`] from the given number of milliseconds. Panics on overflow.
    pub fn from_millis(millis: u64) -> Duration {
        Duration::try_from_millis(millis).expect("duration overflow")
    }

    /// Constructs a [`Duration`] from the given number of nanoseconds.
    pub const fn from_nanos(nanos: u64) -> Duration {
        Self { nanos }
    }

    /// Constructs a [`Duration`] for the given number of whole seconds or returns [`None`] if
    /// this exceeds the representable range of [`Duration`].
    pub fn try_from_secs(secs: u64) -> Option<Duration> {
        secs.checked_mul(NANOS_PER_SEC).map(Duration::from_nanos)
    }

    /// Constructs a [`Duration`] for the given number of whole milliseconds or returns [`None`]
    /// if this exceeds the representable range of [`Duration`].
    pub fn try_from_millis(millis: u64) -> Option<Duration> {
        millis
            .checked_mul(NANOS_PER_MILLI)
            .map(Duration::from_nanos)
    }

    /// Constructs a [`Duration`] for the given number of seconds. Panics on overflow, or if
    /// `secs` is negative.
    pub fn from_secs_scalar(secs: Scalar) -> Duration {
        Duration::try_from_secs_scalar(secs).unwrap()
    }

    /// Tries to construct a [`Duration`] for the given number of seconds.
    pub fn try_from_secs_scalar(secs: Scalar) -> Result<Duration, TryFromFloatError> {
        Duration::try_from_secs_f32(secs)
    }

    /// Tries to construct a [`Duration`] for the given number of seconds.
    pub fn try_from_secs_f32(secs: f32) -> Result<Duration, TryFromFloatError> {
        if secs < 0.0 {
            Err(TryFromFloatError::Negative)
        } else {
            let src = std::time::Duration::try_from_secs_f32(secs)
                .map_err(|_| TryFromFloatError::OverflowOrNan)?;
            Ok(src.try_into()?)
        }
    }

    /// Tries to construct a [`Duration`] for the given number of seconds.
    pub fn try_from_secs_f64(secs: f64) -> Result<Duration, TryFromFloatError> {
        if secs < 0.0 {
            Err(TryFromFloatError::Negative)
        } else {
            let src = std::time::Duration::try_from_secs_f64(secs)
                .map_err(|_| TryFromFloatError::OverflowOrNan)?;
            Ok(src.try_into()?)
        }
    }

    /// Checked `Duration` addition. Computes `self + other`, returning [`None`]
    /// if overflow occurred.
    pub fn checked_add(self, rhs: Duration) -> Option<Duration> {
        self.nanos.checked_add(rhs.nanos).map(Duration::from_nanos)
    }

    /// Combines [`Duration::checked_add`] and [`Duration::try_from_secs_scalar`], returning
    /// [`None`] if overflow occurred.
    pub fn checked_add_from_secs_scalar(self, rhs: Scalar) -> Option<Duration> {
        self.checked_add(Duration::try_from_secs_scalar(rhs).ok()?)
    }

    /// Checked `Duration` subtraction. Computes `self - other`, returning [`None`]
    /// if the result would be negative or if overflow occurred.
    pub fn checked_sub(self, rhs: Duration) -> Option<Duration> {
        self.nanos.checked_sub(rhs.nanos).map(Duration::from_nanos)
    }

    /// Indicates whether this is [`Duration::ZERO`].
    pub fn is_zero(self) -> bool {
        self.nanos == 0
    }

    /// Gets the total number of seconds in this [`Duration`] as a `f64`.
    pub fn as_secs_f64(self) -> f64 {
        (self.nanos as f64) / (NANOS_PER_SEC as f64)
    }

    /// Gets the total number of seconds in this [`Duration`] as a scalar.
    pub fn as_secs_scalar(self) -> Scalar {
        (self.nanos as Scalar) / (NANOS_PER_SEC as Scalar)
    }

    /// Gets the total number of nanoseconds in this [`Duration`].
    pub fn as_nanos(self) -> u64 {
        self.nanos
    }
}

impl From<Duration> for std::time::Duration {
    fn from(value: Duration) -> Self {
        std::time::Duration::from_nanos(value.nanos)
    }
}

impl TryFrom<std::time::Duration> for Duration {
    type Error = TryFromError;
    fn try_from(value: std::time::Duration) -> Result<Self, TryFromError> {
        Ok(Duration::from_nanos(
            value.as_nanos().try_into().map_err(|_| TryFromError)?,
        ))
    }
}

impl Add for Duration {
    type Output = Duration;
    fn add(self, rhs: Duration) -> Duration {
        self.checked_add(rhs)
            .expect("overflow when adding durations")
    }
}

impl AddAssign for Duration {
    fn add_assign(&mut self, rhs: Duration) {
        *self = *self + rhs;
    }
}

impl Sub for Duration {
    type Output = Duration;
    fn sub(self, rhs: Duration) -> Duration {
        self.checked_sub(rhs)
            .expect("overflow when subtracting durations")
    }
}

impl SubAssign for Duration {
    fn sub_assign(&mut self, rhs: Duration) {
        *self = *self - rhs;
    }
}

impl Div<usize> for Duration {
    type Output = Duration;
    fn div(self, rhs: usize) -> Duration {
        Duration::from_nanos(self.nanos / (rhs as u64))
    }
}

impl DivAssign<usize> for Duration {
    fn div_assign(&mut self, rhs: usize) {
        *self = *self / rhs;
    }
}

impl std::fmt::Debug for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <std::time::Duration as std::fmt::Debug>::fmt(&(*self).into(), f)
    }
}

/// An error which can be returned when converting a floating-point value of seconds into a
/// [`Duration`].
#[derive(thiserror::Error, Debug)]
pub enum TryFromFloatError {
    #[error("value is negative")]
    Negative,
    #[error("duration overflow")]
    OverflowOrNan,
}

impl From<TryFromError> for TryFromFloatError {
    fn from(_: TryFromError) -> Self {
        Self::OverflowOrNan
    }
}

/// An error which can be returned when converting a [`std::time::Duration`] into a [`Duration`].
#[derive(thiserror::Error, Debug)]
#[error("duration overflow")]
pub struct TryFromError;