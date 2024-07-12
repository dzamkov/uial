use crate::{Property, PropertyBase};
use std::cell::Cell;

/// An object which tracks "observations" and changes made to it. This is useful for caching,
/// since it provides a way for checking when the data used to produce a cached value becomes
/// invalid.
pub trait Track {
    /// Summarizes a set of observations made to this object, providing just enough information
    /// to tell whether an alternate version of the object will yield the same results for those
    /// observations.
    type ValidityToken;

    /// Runs the given function while recording observations made to this object.
    fn track<R>(&self, inner: impl FnOnce() -> R) -> (R, Self::ValidityToken);

    /// Determines whether the observations represented by the given validity token are valid in
    /// this object's current state. This may spuriously return `false`, depending on the
    /// granularity of observation tracking. Note that if `is_valid` returns `true`, that is itself
    /// considered an observation, and will be recorded by an enclosing [`Track::track`].
    fn is_valid(&self, token: &Self::ValidityToken) -> bool;
}

impl<A: Track, B: Track> Track for (A, B) {
    type ValidityToken = (A::ValidityToken, B::ValidityToken);

    fn track<R>(&self, inner: impl FnOnce() -> R) -> (R, Self::ValidityToken) {
        let ((res, token_1), token_0) = self.0.track(|| self.1.track(inner));
        (res, (token_0, token_1))
    }

    fn is_valid(&self, token: &Self::ValidityToken) -> bool {
        self.0.is_valid(&token.0) && self.1.is_valid(&token.1)
    }
}

/// Caches a value of type `T` which is computed from an environment of type `Env`.
pub struct Cache<Env: Track + ?Sized, T>(Cell<Option<(Env::ValidityToken, T)>>);

impl<Env: Track + ?Sized, T> Cache<Env, T> {
    /// Constructs a new, initially-empty [`Cache`].
    pub fn new() -> Self {
        Self(Cell::new(None))
    }

    /// Accesses the contents of this [`Cache`], (re-)computing them if necessary.
    pub fn with<R>(
        &self,
        env: &Env,
        compute: impl FnOnce(&Env, Option<T>) -> T,
        f: impl FnOnce(&T) -> R,
    ) -> R {
        let old = if let Some((token, value)) = self.0.take() {
            if env.is_valid(&token) {
                let res = f(&value);
                self.0.set(Some((token, value)));
                return res;
            } else {
                Some(value)
            }
        } else {
            None
        };
        let (value, token) = env.track(|| compute(env, old));
        let res = f(&value);
        self.0.set(Some((token, value)));
        res
    }

    /// Gets the current value of this [`Cache`], (re-)computing them if necessary.
    pub fn get(&self, env: &Env, compute: impl FnOnce(&Env, Option<T>) -> T) -> T
    where
        T: Clone,
    {
        self.with(env, compute, Clone::clone)
    }
}

impl<Env: Track + ?Sized, T> Default for Cache<Env, T> {
    fn default() -> Self {
        Self::new()
    }
}

/// An implementation of [`Property`] using a [`Cache`] and a computation function.
pub struct CacheProperty<Env: Track + ?Sized, F: Fn(&Env, Option<T>) -> T, T> {
    cache: Cache<Env, T>,
    compute: F,
}

impl<Env: Track + ?Sized, F: Fn(&Env, Option<T>) -> T, T> PropertyBase
    for CacheProperty<Env, F, T>
{
    type Value = T;
}

impl<Env: Track + ?Sized, F: Fn(&Env, Option<T>) -> T, T> Property<Env>
    for CacheProperty<Env, F, T>
{
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&T) -> R) -> R {
        self.cache.with(env, &self.compute, inner)
    }
}

/// Constructs a [`Property`] from the given cached computation function.
pub fn cached_prop<Env: Track + ?Sized, F: Fn(&Env, Option<T>) -> T, T>(
    compute: F,
) -> CacheProperty<Env, F, T> {
    CacheProperty {
        cache: Cache::new(),
        compute,
    }
}
