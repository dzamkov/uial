use std::borrow::Cow;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::num::NonZeroU64;

/// Represents the state of an application at a certain moment.
pub trait State {
    /// A state-dependent value of a given type which can be modified directly.
    type Cell<T>;

    /// Creates a cell with the given initial value.
    fn new_cell<T>(&self, value: T) -> Self::Cell<T>;

    /// Gets a reference to a cell's value for this state.
    fn get_cell<'a, T>(&'a self, cell: &'a Self::Cell<T>) -> &'a T;

    /// Modifies a cell's value for this state.
    fn modify_cell<'a, T>(&'a mut self, cell: &'a Self::Cell<T>) -> &'a mut T;

    /// A state-dependent value which is deterministically computed using a function. This enables
    /// caching of the underlying value.
    type Derived<F: Fn(&Self) -> T, T: Clone>;

    /// Creates a new derived value which is computed by the given function.
    fn new_derived<F: Fn(&Self) -> T, T: Clone>(&self, compute_fn: F) -> Self::Derived<F, T>;

    /// Gets a derived value for this states.
    fn get_derived<'a, F: Fn(&Self) -> T, T: Clone>(
        &'a self,
        derived: &'a Self::Derived<F, T>,
    ) -> Cow<'a, T>;
}

/// A simple implementation of [`State`] which uses an internal version counter (i.e. the "clock")
/// to track invalidation. This is fairly inefficient, since changing any state-dependent value
/// will invalidate all derived values.
/// 
/// The `'brand` lifetime serves a similar purpose as that of
/// [`GhostToken`](https://docs.rs/ghost-cell/0.2.2/ghost_cell/ghost_cell/struct.GhostToken.html).
pub struct ClockState<'brand> {
    clock: NonZeroU64,
    marker: PhantomData<&'brand ()>,
}

/// A state-dependent value within the context of a [`ClockState`].
pub struct ClockCell<'brand, T> {
    value: UnsafeCell<T>,
    marker: PhantomData<&'brand ()>,
}

/// A state-dependent derived value within the context of a [`ClockState`].
pub struct ClockDerived<'brand, F, T> {
    cache: UnsafeCell<Option<(NonZeroU64, T)>>,
    compute_fn: F,
    marker: PhantomData<&'brand ()>,
}

impl<'brand> ClockState<'brand> {
    /// Creates a new [`ClockState`].
    pub fn new<R>(func: impl for<'a> FnOnce(ClockState<'a>) -> R) -> R {
        func(ClockState {
            clock: unsafe { NonZeroU64::new_unchecked(1) },
            marker: PhantomData,
        })
    }

    /// Creates a new [`ClockState`] with an arbitrary `'brand`. This provides no mechanism for
    /// ensuring that state-dependent values aren't used across different states, so it is up to
    /// the caller to ensure this fact.
    pub unsafe fn new_unchecked() -> Self {
        ClockState {
            clock: NonZeroU64::new_unchecked(1),
            marker: PhantomData,
        }
    }
}

impl<'brand> State for ClockState<'brand> {
    type Cell<T> = ClockCell<'brand, T>;

    fn new_cell<T>(&self, value: T) -> Self::Cell<T> {
        ClockCell {
            value: UnsafeCell::new(value),
            marker: PhantomData
        }
    }

    fn get_cell<T>(&self, cell: &Self::Cell<T>) -> &T {
        unsafe { &*cell.value.get() }
    }

    fn modify_cell<T>(&mut self, cell: &Self::Cell<T>) -> &mut T {
        self.clock = self.clock.checked_add(1).unwrap();
        unsafe { &mut *cell.value.get() }
    }

    type Derived<F: Fn(&Self) -> T, T: Clone> = ClockDerived<'brand, F, T>;

    fn new_derived<F: Fn(&Self) -> T, T: Clone>(&self, compute_fn: F) -> Self::Derived<F, T> {
        ClockDerived {
            cache: UnsafeCell::new(None),
            compute_fn,
            marker: PhantomData
        }
    }

    fn get_derived<F: Fn(&Self) -> T, T: Clone>(&self, derived: &Self::Derived<F, T>) -> Cow<T> {
        todo!()
    }
}
