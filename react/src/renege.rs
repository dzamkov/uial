use super::*;
use ::renege;
use ghost_cell::{GhostCell, GhostToken};
use std::cell::Cell;

// Re-export `renege` crate.
pub use ::renege::*;

/// An implementation of [`React`] and [`Track`] based on [`GhostToken`] and [`renege`].
pub struct RenegeReact<'brand> {
    token: GhostToken<'brand>,
    validity: Cell<renege::Token>,
}

impl RenegeReact<'_> {
    /// Constructs a new [`RenegeReact`] with the `'static` brand.
    ///
    /// ## Safety
    /// It is not safe to mix [`ReactCell`]s between different [`RenegeReact`]s. Usually this
    /// would be checked by generating a unique `'brand` for each [`RenegeReact`] and tying the
    /// [`ReactCell`] to that brand. However, this function allows multiple [`RenegeReact`]s
    /// to be created with the same `'brand`. Thus, it is up to the caller to ensure that
    /// [`ReactCell`] are not mixed between [`RenegeReact`]s.
    pub unsafe fn new_static() -> RenegeReact<'static> {
        RenegeReact {
            token: unsafe { std::mem::zeroed() },
            validity: Cell::new(renege::Token::never()),
        }
    }

    /// Manually declares a dependency of the current computation on the given token.
    pub fn depends_on(&self, token: renege::Token) {
        let mut validity = self.validity.get();
        validity &= token;
        self.validity.set(validity);
    }

    /// Gets the [`Cell`] which defines the validity of the current computation.
    pub fn validity(&self) -> &Cell<renege::Token> {
        &self.validity
    }
}

impl<'brand> React for RenegeReact<'brand> {
    type RawCell<T> = GhostCell<'brand, (T, renege::Condition)>;

    fn new_cell<T>(&self, value: T) -> ReactCell<Self, T> {
        ReactCell::from_raw(GhostCell::new((value, renege::Condition::new())))
    }

    fn with_cell_ref<T, R>(&self, cell: &ReactCell<Self, T>, f: impl FnOnce(&T) -> R) -> R {
        let (cell, cond) = cell.as_raw().borrow(&self.token);
        let mut validity = self.validity.get();
        validity &= cond.token();
        self.validity.set(validity);
        f(cell)
    }

    fn with_cell_mut<T, R>(&mut self, cell: &ReactCell<Self, T>, f: impl FnOnce(&mut T) -> R) -> R {
        let (cell, cond) = cell.as_raw().borrow_mut(&mut self.token);
        *cond = renege::Condition::new();
        self.validity.set(renege::Token::never());
        f(cell)
    }

    fn set_cell<T>(&mut self, cell: &ReactCell<Self, T>, value: T) {
        // Unlike `with_cell_mut`, the validity token is not invalidated since the
        // value is not read.
        let (cell, cond) = cell.as_raw().borrow_mut(&mut self.token);
        *cond = renege::Condition::new();
        *cell = value;
    }
}

impl Track for RenegeReact<'_> {
    type ValidityToken = renege::Token;

    fn track<R>(&self, inner: impl FnOnce() -> R) -> (R, renege::Token) {
        let mut validity = self.validity.get();
        self.validity.set(renege::Token::always());
        let res = inner();
        let res_validity = self.validity.get();
        validity &= res_validity;
        self.validity.set(validity);
        (res, res_validity)
    }

    fn is_valid(&self, token: &renege::Token) -> bool {
        if token.is_valid() {
            let mut validity = self.validity.get();
            validity &= *token;
            self.validity.set(validity);
            true
        } else {
            false
        }
    }
}
