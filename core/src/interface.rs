use crate::*;
use std::mem::transmute;

/// A direct wrapper over a value of type `T` which allows methods from common interfaces to be
/// called even in situations where the exact type of `T` is unknown. This is mainly useful for
/// closure parameters whose type won't be inferred until after the closure is defined.
#[repr(transparent)]
pub struct Interface<T>(T);

impl<T> Interface<T> {
    /// Gets an immutable [`Interface`] wrapper over the given interface.
    pub fn from_ref(source: &T) -> &Self {
        unsafe { transmute(source) }
    }
    
    /// Gets a mutable [`Interface`] wrapper over the given interface.
    pub fn from_mut(source: &mut T) -> &mut Self {
        unsafe { transmute(source) }
    }
}

impl<T> AsRef<T> for Interface<T> {
    fn as_ref(&self) -> &T {
        unsafe { transmute(self) }
    }
}

impl<T> AsMut<T> for Interface<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { transmute(self) }
    }
}

impl<S: State> Interface<S> {
    /// Calls [`State::new_cell`].
    pub fn new_cell<T: Clone>(&self, value: T) -> StateCell<S, T> {
        self.as_ref().new_cell(value)
    }

    /// Calls [`State::get_cell`].
    pub fn get_cell<'a, T: Clone>(&'a self, cell: &'a StateCell<S, T>) -> &'a T {
        self.as_ref().get_cell(cell)
    }

    /// Calls [`State::modify_cell`].
    pub fn modify_cell<'a, T: Clone>(&'a mut self, cell: &'a StateCell<S, T>) -> &'a mut T {
        self.as_mut().modify_cell(cell)
    }

    /// Calls [`State::get_derived`].
    pub fn get_derived<'a, D: OwnDependent<S>>(
        &'a self,
        cached: &'a StateDerived<S, D>,
    ) -> std::borrow::Cow<'a, D::Target> {
        self.as_ref().get_derived(cached)
    }
}

impl<G: Graphics> Graphics for Interface<G> {
    type Image = G::Image;
    type Drawer<'a> = G::Drawer<'a>;
    fn load_image(&self, data: image::DynamicImage) -> Self::Image {
        self.as_ref().load_image(data)
    }
}