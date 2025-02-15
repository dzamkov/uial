use super::*;
pub use crate::geometry::{Duration, Instant};
use std::cell::Cell;

/// An "environment" which provides an absolute time reference.
pub trait HasClock {
    /// Gets the current time in this environment. It is guaranteed that any changes made in the
    /// environment at this time will not be seen in any environment with an earlier time.
    fn clock(&self) -> Instant;
}

impl<A, B: HasClock> HasClock for (A, B) {
    fn clock(&self) -> Instant {
        self.1.clock()
    }
}

/// A minimal environment which provides an absolute time reference (i.e. implements [`HasClock`])
/// and is [`Track`].
pub struct Clock {
    time: Instant,
    valid_to: Cell<Instant>,
}

impl Clock {
    /// Constructs a new [`Clock`] set to the given initial time.
    pub fn new(time: Instant) -> Self {
        Self {
            time,
            valid_to: Cell::new(Instant::MAX),
        }
    }

    /// Gets the time for this [`Clock`].
    pub fn get(&self) -> Instant {
        self.valid_to.set(self.time);
        self.time
    }

    /// Sets the time for this [`Clock`].
    pub fn set(&mut self, time: Instant) {
        self.time = time;
    }
}

impl HasClock for Clock {
    fn clock(&self) -> Instant {
        self.get()
    }
}

impl Track for Clock {
    type ValidityToken = Instant;

    fn track<R>(&self, inner: impl FnOnce() -> R) -> (R, Self::ValidityToken) {
        let cur_valid_to = self.valid_to.get();
        self.valid_to.set(Instant::MAX);
        let res = inner();
        let res_valid_to = self.valid_to.get();
        self.valid_to.set(std::cmp::min(cur_valid_to, res_valid_to));
        (res, res_valid_to)
    }

    fn is_valid(&self, token: &Self::ValidityToken) -> bool {
        self.time <= *token
    }
}

/// A snapshot of a dynamic object or system that evolves over time, without dependence on an
/// external environment.
pub trait Update {
    /// Advances the state of the dynamic object by the given amount of time, assuming no
    /// external influences. Calling this twice with two separate [`Duration`]s should be
    /// approximately equivalent to calling it once with the combined [`Duration`].
    fn update(&mut self, d_time: Duration);
}

/// Describes the evolution of a value of [`Update`]able type `T` in a "free trajectory", starting
/// at a certain [`Instant`].
pub struct Trajectory<T: Update> {
    pub start_inst: Instant,
    pub start_value: T,
}

impl<T: Update + Clone> Trajectory<T> {
    /// Constructs a new [`Trajectory`] starting at the given [`Instant`] with the given state.
    pub fn new(start_inst: Instant, start_value: T) -> Self {
        Self {
            start_inst,
            start_value,
        }
    }

    /// Accesses the value of this [`Trajectory`] at the given instant, or returns [`None`] if the
    /// instant is out of the defined time range of the trajectory.
    pub fn at<R>(&self, inst: Instant, inner: impl FnOnce(&T) -> R) -> Option<R> {
        let d_time = inst.checked_sub(self.start_inst)?;
        let mut value = self.start_value.clone();
        value.update(d_time);
        Some(inner(&value))
    }

    /// Modifies the value of this [`Trajectory`] at the given instant, or returns [`None`] if the
    /// instant out of the defined time range of the trajectory.
    pub fn at_mut<R>(&mut self, inst: Instant, inner: impl FnOnce(&mut T) -> R) -> Option<R> {
        let d_time = inst.checked_sub(self.start_inst)?;
        self.start_value.update(d_time);
        self.start_inst = inst;
        Some(inner(&mut self.start_value))
    }
}

/// A [`Property`]/[`Field`] which represents the "current" value of a [`Trajectory`] according to
/// the environment's clock time (see [`HasClock`]).
pub struct Current<S>(S);

impl<S: PropertyBase<Value = Trajectory<T>>, T: Update> PropertyBase for Current<S> {
    type Value = T;
}

impl<Env: ?Sized + HasClock, S: Property<Env, Value = Trajectory<T>>, T: Update + Clone>
    Property<Env> for Current<S>
{
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&T) -> R) -> R {
        let clock = env.clock();
        self.0.with_ref(env, |traj| {
            traj.at(clock, inner).expect("Clock monotonicity violation")
        })
    }
}

impl<Env: ?Sized + HasClock, S: Field<Env, Value = Trajectory<T>>, T: Update + Clone> Field<Env>
    for Current<S>
{
    fn with_mut<R>(&self, env: &mut Env, inner: impl FnOnce(&mut T) -> R) -> R {
        let clock = env.clock();
        self.0.with_mut(env, |traj| {
            traj.at_mut(clock, inner)
                .expect("Clock monotonicity violation")
        })
    }
}

/// Contains [`Current`]-related extension methods for [`PropertyBase`].
pub trait CurrentExt: Sized + PropertyBase {
    /// Gets the "current" value of this [`Trajectory`] field or property according to the
    /// environment's clock time (see [`HasClock`]).
    fn current(self) -> Current<Self> {
        Current(self)
    }
}

impl<S: PropertyBase<Value = Trajectory<T>>, T: Update> CurrentExt for S {}
