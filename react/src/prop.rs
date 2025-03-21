use std::rc::Rc;
use std::sync::Arc;

/// Identifies a value of type `Self::Value` which can be obtained from an "environment"
/// of an unspecified type.
pub trait PropertyLike {
    /// The type of value for this property.
    type Value: ?Sized;
}

/// Identifies a value of type `Self::Value` which can be obtained from an "environment" of type
/// `Env`.
pub trait Property<Env: ?Sized>: PropertyLike {
    /// Accesses the value of this property using the given closure.
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&Self::Value) -> R) -> R;

    /// Gets the value of this [`Property`] from the given environment.
    fn get(&self, env: &Env) -> <Self::Value as ToOwned>::Owned
    where
        Self::Value: ToOwned,
    {
        Property::with_ref(self, env, |x| x.to_owned())
    }
}

/// Identifies a mutable value of type `Self::Value` which is stored in an "environment" of type
/// `Env`.
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

impl<T: PropertyLike> PropertyLike for &T {
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

impl<T: PropertyLike> PropertyLike for Rc<T> {
    type Value = T::Value;
}

impl<Env: ?Sized, T: Property<Env>> Property<Env> for Rc<T> {
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&T::Value) -> R) -> R {
        (**self).with_ref(env, inner)
    }
}

impl<Env: ?Sized, T: Field<Env>> Field<Env> for Rc<T> {
    fn with_mut<R>(&self, env: &mut Env, inner: impl FnOnce(&mut T::Value) -> R) -> R {
        (**self).with_mut(env, inner)
    }
}

impl<T: PropertyLike> PropertyLike for Arc<T> {
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
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Const<T>(pub T);

impl<T> PropertyLike for Const<T> {
    type Value = T;
}

impl<Env: ?Sized, T> Property<Env> for Const<T> {
    fn with_ref<R>(&self, _: &Env, inner: impl FnOnce(&T) -> R) -> R {
        inner(&self.0)
    }
}

/// A [`Property`] wrapper over a constant reference.
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ConstRef<'a, T: ?Sized>(pub &'a T);

impl<T: ?Sized> PropertyLike for ConstRef<'_, T> {
    type Value = T;
}

impl<Env: ?Sized, T: ?Sized> Property<Env> for ConstRef<'_, T> {
    fn with_ref<R>(&self, _: &Env, inner: impl FnOnce(&T) -> R) -> R {
        inner(self.0)
    }
}

/// A [`Property`] of an unspecified type, determined at runtime.
pub struct DynProperty<'a, Env: ?Sized, T: ?Sized>(dyn PropertySafe<Env, T> + 'a);

impl<'a, Env: ?Sized, T: ?Sized> DynProperty<'a, Env, T> {
    /// Converts a reference to a [`Property`] into a reference to a [`DynProperty`].
    pub fn from_ref<P: Property<Env, Value = T> + 'a>(source: &P) -> &Self {
        let source: &(dyn PropertySafe<Env, T> + 'a) = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts a boxed [`Property`] into a boxed [`DynProperty`].
    pub fn from_box<P: Property<Env, Value = T> + 'a>(source: Box<P>) -> Box<Self> {
        let source: Box<dyn PropertySafe<Env, T> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts an [`Rc`]-wrapped [`Property`] into a boxed [`DynProperty`].
    pub fn from_rc<P: Property<Env, Value = T> + 'a>(source: Rc<P>) -> Rc<Self> {
        let source: Rc<dyn PropertySafe<Env, T> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts an [`Arc`]-wrapped [`Property`] into a boxed [`DynProperty`].
    pub fn from_arc<P: Property<Env, Value = T> + 'a>(source: Arc<P>) -> Arc<Self> {
        let source: Arc<dyn PropertySafe<Env, T> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }
}

/// An object-safe variant of [`Property`].
trait PropertySafe<Env: ?Sized, T: ?Sized> {
    /// Accesses the value of this property using the given closure.
    fn with_ref(&self, env: &Env, inner: &mut dyn FnMut(&T));

    /// Gets the value of this [`Property`] from the given environment.
    fn get(&self, env: &Env) -> T::Owned
    where
        T: ToOwned;
}

impl<Env: ?Sized, T: ?Sized, P: Property<Env, Value = T>> PropertySafe<Env, T> for P {
    fn with_ref(&self, env: &Env, inner: &mut dyn FnMut(&T)) {
        self.with_ref(env, inner)
    }

    fn get(&self, env: &Env) -> T::Owned
    where
        T: ToOwned,
    {
        self.get(env)
    }
}

impl<Env: ?Sized, T: ?Sized> PropertyLike for DynProperty<'_, Env, T> {
    type Value = T;
}

impl<Env: ?Sized, T: ?Sized> Property<Env> for DynProperty<'_, Env, T> {
    fn with_ref<R>(&self, env: &Env, inner: impl FnOnce(&Self::Value) -> R) -> R {
        let mut inner = Some(inner);
        let mut res = None;
        {
            let inner = &mut inner;
            let res = &mut res;
            self.0.with_ref(env, &mut |value| {
                *res = Some(inner.take().expect("closure called twice")(value));
            });
        }
        res.expect("closure not called")
    }

    fn get(&self, env: &Env) -> T::Owned
    where
        T: ToOwned,
    {
        self.0.get(env)
    }
}