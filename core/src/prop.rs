use std::marker::PhantomData;
use std::sync::Arc;

/// Identifies a value of a certain type which can be obtained from an "environment". See
/// [`Property`].
pub trait PropertyBase {
    /// The type of value for this property.
    type Value: ?Sized;

    /// Constructs a property which "selects" a specific sub-field from this property.
    fn select<F: Fn(&Self::Value) -> &T, T>(self, f: F) -> SelectProperty<Self, F>
    where
        Self: Sized,
    {
        SelectProperty {
            source: self,
            select: f,
        }
    }
}

/// Identifies a value of a certain type which can be obtained from an "environment" of type `Env`.
pub trait Property<Env: ?Sized>: PropertyBase {
    /// Accesses the value of this property using the given closure.
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&Self::Value) -> R) -> R;

    /// Gets the value of this [`Property`] from the given environment.
    fn get(&self, env: &Env) -> <Self::Value as ToOwned>::Owned
    where
        Self::Value: ToOwned,
    {
        self.with_ref(env, |x| x.to_owned())
    }
}

/// Identifies a mutable value of type `T` which is stored in an `Env`.
pub trait Field<Env: ?Sized>: Property<Env> {
    /// Accesses the value of this field using the given closure.
    fn with_mut<R>(&self, env: &mut Env, inner: impl FnOnce(&mut Self::Value) -> R) -> R;

    /// Sets the value of this field.
    fn set(&self, env: &mut Env, value: Self::Value)
    where
        Self::Value: Sized,
    {
        self.with_mut(env, |x| *x = value)
    }

    /// Takes the value of this field, leaving [`Default::default`] in its place.
    fn take(&self, env: &mut Env) -> Self::Value
    where
        Self::Value: Default,
    {
        self.with_mut(env, std::mem::take)
    }
}

impl<T: PropertyBase> PropertyBase for &T {
    type Value = T::Value;
}

impl<Env: ?Sized, T: Property<Env>> Property<Env> for &T {
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&T::Value) -> R) -> R {
        (**self).with_ref(env, inner)
    }
}

impl<Env: ?Sized, T: Field<Env>> Field<Env> for &T {
    fn with_mut<R>(&self, env: &mut Env, inner: impl FnOnce(&mut T::Value) -> R) -> R {
        (**self).with_mut(env, inner)
    }
}

impl<T: PropertyBase> PropertyBase for Arc<T> {
    type Value = T::Value;
}

impl<Env: ?Sized, T: Property<Env>> Property<Env> for Arc<T> {
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&T::Value) -> R) -> R {
        (**self).with_ref(env, inner)
    }
}

impl<Env: ?Sized, T: Field<Env>> Field<Env> for Arc<T> {
    fn with_mut<R>(&self, env: &mut Env, inner: impl FnOnce(&mut T::Value) -> R) -> R {
        (**self).with_mut(env, inner)
    }
}

/// A [`Property`] wrapper over a constant value.
#[derive(Clone, Copy)]
pub struct Const<T>(pub T);

/// Shortcut for [`Const::new`].
pub fn const_<T>(value: T) -> Const<T> {
    Const(value)
}

impl<T> PropertyBase for Const<T> {
    type Value = T;
}

impl<Env: ?Sized, T> Property<Env> for Const<T> {
    fn with_ref<R>(&self, _: &Env, inner: impl FnOnce(&T) -> R) -> R {
        inner(&self.0)
    }
}

/// A [`Field`] which represents the environment itself.
pub struct Identity<Env: ?Sized>(PhantomData<fn(Env) -> Env>);

impl<Env: ?Sized> Identity<Env> {
    /// Constructs a new [`Identity`].
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Env: ?Sized> Default for Identity<Env> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Env: ?Sized> Copy for Identity<Env> {}

impl<Env: ?Sized> Clone for Identity<Env> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Env: ?Sized> PropertyBase for Identity<Env> {
    type Value = Env;
}

impl<Env: ?Sized> Property<Env> for Identity<Env> {
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&Env) -> R) -> R {
        inner(env)
    }
}

impl<Env: ?Sized> Field<Env> for Identity<Env> {
    fn with_mut<R>(&self, env: &mut Env, inner: impl FnOnce(&mut Env) -> R) -> R {
        inner(env)
    }
}

/// A [`Property`] which "selects" a sub-field of a source [`Property`].
pub struct SelectProperty<P, F> {
    source: P,
    select: F,
}

impl<P: PropertyBase, F: Fn(&P::Value) -> &T, T> PropertyBase for SelectProperty<P, F> {
    type Value = T;
}

impl<Env: ?Sized, P: Property<Env>, F: Fn(&P::Value) -> &T, T> Property<Env>
    for SelectProperty<P, F>
{
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&T) -> R) -> R {
        self.source
            .with_ref(env, |source| inner((self.select)(source)))
    }
}

/// A [`Field`] implemented using a pair of closures.
pub struct FnField<P, GetRef, GetMut> {
    parent: P,
    get_ref: GetRef,
    get_mut: GetMut,
}

impl<P: PropertyBase, GetRef: Fn(&P::Value) -> &T, GetMut: Fn(&mut P::Value) -> &mut T, T>
    FnField<P, GetRef, GetMut>
{
    /// Constructs a new [`FnField`].
    pub fn new(parent: P, get_ref: GetRef, get_mut: GetMut) -> Self {
        Self {
            parent,
            get_ref,
            get_mut,
        }
    }
}

impl<P: PropertyBase, GetRef: Fn(&P::Value) -> &T, GetMut: Fn(&mut P::Value) -> &mut T, T>
    PropertyBase for FnField<P, GetRef, GetMut>
{
    type Value = T;
}

impl<
        Env: ?Sized,
        P: Property<Env>,
        GetRef: Fn(&P::Value) -> &T,
        GetMut: Fn(&mut P::Value) -> &mut T,
        T,
    > Property<Env> for FnField<P, GetRef, GetMut>
{
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&Self::Value) -> R) -> R {
        let parent = &self.parent;
        parent.with_ref(env, |parent| inner((self.get_ref)(parent)))
    }
}

impl<
        Env: ?Sized,
        P: Field<Env>,
        GetRef: Fn(&P::Value) -> &T,
        GetMut: Fn(&mut P::Value) -> &mut T,
        T,
    > Field<Env> for FnField<P, GetRef, GetMut>
{
    fn with_mut<R>(&self, env: &mut Env, inner: impl FnOnce(&mut Self::Value) -> R) -> R {
        let parent = &self.parent;
        parent.with_mut(env, |parent| inner((self.get_mut)(parent)))
    }
}

#[macro_export]
macro_rules! field {
    ($head:tt $($tail:tt)*) => {
        $crate::ui::FnField::new(
            $head,
            |parent: &_| & parent $($tail)*,
            |parent: &mut _| &mut parent $($tail)*,
        )
    };
}