use std::borrow::Cow;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::num::NonZeroU64;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::Arc;

/// Represents the state of an application at a certain moment.
pub trait State {
    /// A state-dependent value of a given type which can be modified directly.
    type Cell<T: Clone>;

    /// A cache for a state-dependent value.
    type Cache<T: Clone>: Default;

    /// Creates a cell with the given initial value.
    fn new_cell<T: Clone>(&self, value: T) -> StateCell<Self, T>;

    /// Gets a reference to a cell's value for this state.
    fn get_cell<'a, T: Clone>(&'a self, cell: &'a StateCell<Self, T>) -> &'a T;

    /// Modifies a cell's value for this state.
    fn modify_cell<'a, T: Clone>(&'a mut self, cell: &'a StateCell<Self, T>) -> &'a mut T;

    /// Sets the the value of a cell for this state.
    fn set_cell<T: Clone>(&mut self, cell: &StateCell<Self, T>, value: T) {
        *self.modify_cell(cell) = value;
    }

    /// Gets a cached value for this state.
    fn get_derived<'a, D: DerivedFn<Self>>(
        &'a self,
        cached: &'a StateDerived<Self, D>,
    ) -> Cow<'a, D::Target>;
}

/// Contains caching-related extension methods for [`State`].
pub trait StateCacheExt: State {
    /// Creates a new cached value based on the given [`DerivedFn`]. Note that a reference to
    /// the [`State`] is not required to create a cache and this method only exists to assist
    /// type inference. Use `new` on [`StateDerived`] to create the cache directly from `source`.
    fn new_derived<D: DerivedFn<Self>>(&self, source: D) -> StateDerived<Self, D> {
        StateDerived::new(source)
    }

    /// Creates a new cached value based on the given function. Note that a reference to
    /// the [`State`] is not required to create a cache and this method only exists to assist
    /// type inference. Use `new` on [`StateDerived`] to create the cache directly from `source`.
    fn new_derived_fn<'a, T: Clone>(
        &self,
        source: impl Fn(&Self) -> T + 'a,
    ) -> StateDerived<Self, Box<dyn Fn(&Self) -> T + 'a>> {
        StateDerived::new(Box::new(source) as Box<dyn Fn(&Self) -> T>)
    }
}

impl<T: State> StateCacheExt for T {}

/// A [`State`]-dependent value of type `T` which can be modified directly.
pub struct StateCell<S: State + ?Sized, T: Clone>(S::Cell<T>);

impl<S: State + ?Sized, T: Clone> StateCell<S, T> {
    /// Constructs a new [`StateCell`] from the given internal representation.
    pub fn new(source: S::Cell<T>) -> Self {
        StateCell(source)
    }
}

impl<S: State + ?Sized, T: Clone> Deref for StateCell<S, T> {
    type Target = S::Cell<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S: State + ?Sized, T: Clone> DerefMut for StateCell<S, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for StateCell<S, T> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        Cow::Borrowed(s.get_cell(self))
    }
}

impl<'a, S: State + ?Sized, T: Clone> Dependent<S, T> for &'a StateCell<S, T> {
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for Box<StateCell<S, T>> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for Rc<StateCell<S, T>> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for Arc<StateCell<S, T>> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

/// A cached [`State`]-dependent value.
pub struct StateDerived<S: State + ?Sized, D: DerivedFn<S>, T: Clone = <D as DerivedFn<S>>::Target>
{
    source: D,
    cache: S::Cache<T>,
}

impl<S: State + ?Sized, D: DerivedFn<S>> StateDerived<S, D> {
    /// Constructs a new [`StateDerived`] based on the given [`DerivedFn`].
    pub fn new(source: D) -> Self {
        StateDerived {
            source,
            cache: S::Cache::<D::Target>::default(),
        }
    }

    /// Gets the [`DerivedFn`] from which the value of this [`StateDerived`] is derived.
    pub fn source(&self) -> &D {
        &self.source
    }

    /// Gets the cache for this [`StateDerived`].
    pub fn cache(&self) -> &S::Cache<D::Target> {
        &self.cache
    }
}

impl<S: State + ?Sized, D: DerivedFn<S, Target = T>, T: Clone> Dependent<S, T>
    for StateDerived<S, D, T>
{
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        s.get_derived(self)
    }
}

impl<'a, S: State + ?Sized, D: DerivedFn<S, Target = T>, T: Clone> Dependent<S, T>
    for &'a StateDerived<S, D, T>
{
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, D: DerivedFn<S, Target = T>, T: Clone> Dependent<S, T>
    for Box<StateDerived<S, D, T>>
{
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, D: DerivedFn<S, Target = T>, T: Clone> Dependent<S, T>
    for Rc<StateDerived<S, D, T>>
{
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, D: DerivedFn<S, Target = T>, T: Clone> Dependent<S, T>
    for Arc<StateDerived<S, D, T>>
{
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

/// A (potentially) [`State`]-dependent value of a certain type.
pub trait Dependent<S: State + ?Sized, T: Clone> {
    /// Evaluates this dependent value for the given state.
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T>;
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for T {
    fn eval<'a>(&'a self, _: &'a S) -> Cow<'a, T> {
        Cow::Borrowed(self)
    }
}

impl<'a, S: State + ?Sized, T: Clone> Dependent<S, T> for &'a T {
    fn eval<'b>(&'b self, s: &'b S) -> Cow<'b, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for Box<T> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for Rc<T> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

impl<S: State + ?Sized, T: Clone> Dependent<S, T> for Arc<T> {
    fn eval<'a>(&'a self, s: &'a S) -> Cow<'a, T> {
        (**self).eval(s)
    }
}

/// A function which produces a [`State`]-dependent value of a certain type.
pub trait DerivedFn<S: State + ?Sized> {
    /// The type of value produced by this [`DerivedFn`].
    type Target: Clone;

    /// Evaluates this [`DerivedFn`] for the given state.
    fn eval(&self, s: &S) -> Self::Target;
}

impl<S: State + ?Sized, F: Fn(&S) -> T, T: Clone> DerivedFn<S> for F {
    type Target = T;
    fn eval(&self, s: &S) -> Self::Target {
        self(s)
    }
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

/// A state-dependent cached value within the context of a [`ClockState`].
pub struct ClockCache<'brand, T> {
    cache: UnsafeCell<Option<(NonZeroU64, T)>>,
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
    type Cell<T: Clone> = ClockCell<'brand, T>;
    type Cache<T: Clone> = ClockCache<'brand, T>;

    fn new_cell<T: Clone>(&self, value: T) -> StateCell<Self, T> {
        StateCell::new(ClockCell {
            value: UnsafeCell::new(value),
            marker: PhantomData,
        })
    }

    fn get_cell<T: Clone>(&self, cell: &StateCell<Self, T>) -> &T {
        unsafe { &*cell.value.get() }
    }

    fn modify_cell<T: Clone>(&mut self, cell: &StateCell<Self, T>) -> &mut T {
        self.clock = self.clock.checked_add(1).unwrap();
        unsafe { &mut *cell.value.get() }
    }

    fn get_derived<D: DerivedFn<Self>>(&self, derived: &StateDerived<Self, D>) -> Cow<D::Target> {
        // Check cache
        if let Some((clock, value)) = unsafe { &*derived.cache().cache.get() } {
            if *clock == self.clock {
                return Cow::Borrowed(value);
            }
        }

        // Compute value and store in cache
        let value = derived.source.eval(self);
        let cache = unsafe { (*derived.cache().cache.get()).insert((self.clock, value)) };
        Cow::Borrowed(&cache.1)
    }
}

impl<'brand, T> Default for ClockCache<'brand, T> {
    fn default() -> Self {
        ClockCache {
            cache: UnsafeCell::new(None),
            marker: PhantomData,
        }
    }
}

#[test]
fn test_clock_state() {
    ClockState::new(|mut state| {
        let a = state.new_cell(1);
        let b = state.new_cell(2);
        let c = state.new_derived_fn(|state| state.get_cell(&a) + state.get_cell(&b));
        assert_eq!(*state.get_cell::<i32>(&a), 1);
        assert_eq!(*state.get_cell::<i32>(&b), 2);
        assert_eq!(*state.get_derived(&c), 3);
        assert_eq!(*state.get_derived(&c), 3);
        *state.modify_cell(&a) = 3;
        assert_eq!(*state.get_cell(&a), 3);
        assert_eq!(*state.get_derived(&c), 5);
    });
}
