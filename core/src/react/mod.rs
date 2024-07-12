pub mod version;
pub mod renege;

use super::prop::*;
use super::Track;

/// The preferred implementation of [`React`].
pub type DefaultReact = renege::RenegeReact<'static>;

/// Represents an extensible store of state information. More concretely, this allows the creation
/// of [`ReactCell`]s and controls access to their contents, similar to how
/// [`GhostCell`](https://plv.mpi-sws.org/rustbelt/ghostcell/) works. A type that implements
/// both [`React`] and [`Track`] can be viewed as a minimal
/// [FRP](https://en.wikipedia.org/wiki/Functional_reactive_programming) implementation with
/// automatic cache invalidation when state information changes.
pub trait React {
    /// The internal data for a [`ReactCell`] of this [`React`] type.
    type RawCell<T>;

    /// Creates a [`ReactCell`] with the given initial value.
    fn new_cell<T>(&self, value: T) -> ReactCell<Self, T>;

    /// Accesses the current value of the given [`ReactCell`] using the given closure.
    fn with_cell_ref<T, R>(&self, cell: &ReactCell<Self, T>, f: impl FnOnce(&T) -> R) -> R;

    /// Mutably accesses the current value of the given [`ReactCell`] using the given closure.
    fn with_cell_mut<T, R>(&mut self, cell: &ReactCell<Self, T>, f: impl FnOnce(&mut T) -> R) -> R;

    /// Gets the current value of a [`ReactCell`].
    fn get_cell<T: Clone>(&self, cell: &ReactCell<Self, T>) -> T {
        self.with_cell_ref(cell, Clone::clone)
    }

    /// Sets the current value of a [`ReactCell`].
    fn set_cell<T>(&mut self, cell: &ReactCell<Self, T>, value: T) {
        self.with_cell_mut(cell, |x| *x = value)
    }
}

/// A [`React`]-dependent value of type `T`. The value can only be accessed using a reference
/// to the [`React`], `R`.
pub struct ReactCell<R: React + ?Sized, T>(R::RawCell<T>);

impl<R: React + ?Sized, T> ReactCell<R, T> {
    /// Constructs a new [`ReactCell`] with the given initial value.
    pub fn new(state: &R, value: T) -> Self {
        state.new_cell(value)
    }

    /// Constructs a [`ReactCell`] from the given raw data.
    pub fn from_raw(source: R::RawCell<T>) -> Self {
        Self(source)
    }

    /// Gets the raw data for this [`ReactCell`].
    pub fn as_raw(&self) -> &R::RawCell<T> {
        &self.0
    }
}

impl<R: React + ?Sized, T> PropertyBase for ReactCell<R, T> {
    type Value = T;
}

impl<Env: HasReact + ?Sized, T> Property<Env> for ReactCell<Env::React, T> {
    fn with_ref<R>(&self, env: &Env, f: impl FnOnce(&T) -> R) -> R {
        env.react().with_cell_ref(self, f)
    }
}

impl<Env: HasReact + ?Sized, T> Field<Env> for ReactCell<Env::React, T> {
    fn with_mut<R>(&self, env: &mut Env, f: impl FnOnce(&mut T) -> R) -> R {
        env.react_mut().with_cell_mut(self, f)
    }

    fn set(&self, env: &mut Env, value: Self::Value)
    where
        Self::Value: Sized,
    {
        env.react_mut().set_cell(self, value)
    }
}

/// An environment that has a [`React`].
pub trait HasReact {
    /// The type of [`React`] for this environment.
    type React: React + ?Sized;

    /// Gets an immutable reference to the [`React`] for this environment.
    fn react(&self) -> &Self::React;

    /// Gets a mutable reference to the [`React`] for this environment.
    fn react_mut(&mut self) -> &mut Self::React;
}

impl<S: React + ?Sized> HasReact for S {
    type React = Self;

    fn react(&self) -> &Self::React {
        self
    }

    fn react_mut(&mut self) -> &mut Self::React {
        self
    }
}

impl<A: HasReact, B> HasReact for (A, B) {
    type React = A::React;

    fn react(&self) -> &Self::React {
        self.0.react()
    }

    fn react_mut(&mut self) -> &mut Self::React {
        self.0.react_mut()
    }
}