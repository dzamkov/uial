use std::any::{Any, TypeId};
use std::marker::PhantomData;

/// The target location for a downcasting operation. This is essentially a `&'a mut Option<&'b T>`
/// where `T` is a type known only at runtime.
pub struct DowncastRef<'a, 'b> {
    _marker: PhantomData<&'a mut Option<&'b ()>>,
    type_id: TypeId,
    target: *mut (),
}

impl<'a, 'b> DowncastRef<'a, 'b> {
    /// Gets the [`TypeId`] for the target of this reference.
    pub fn type_id(&self) -> TypeId {
        self.type_id
    }

    /// Attempts to set this [`DowncastRef`] to the given value. Returns `false` if the value is of
    /// an incompatible type.
    pub fn put<T: Any + ?Sized>(&mut self, value: &'b T) -> bool {
        if self.type_id == TypeId::of::<T>() {
            unsafe { *(self.target as *mut Option<&'b T>) = Some(value) };
            true
        } else {
            false
        }
    }
}

impl<'a, 'b, T: Any + ?Sized> From<&'a mut Option<&'b T>> for DowncastRef<'a, 'b> {
    fn from(value: &'a mut Option<&'b T>) -> Self {
        Self {
            _marker: PhantomData,
            type_id: TypeId::of::<T>(),
            target: value as *mut Option<&'b T> as *mut ()
        }
    }
}