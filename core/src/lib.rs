#![allow(clippy::bool_assert_comparison)]
pub mod drawer;
mod input;
mod sizing;
mod time;
mod unique;
pub mod widget;
pub mod prelude;
pub mod geometry;

pub use input::*;
pub use sizing::*;
pub use time::*;
use prelude::*;

/// A [`num_rational::Ratio`] of [`u32`]s.
pub type RationalU32 = num_rational::Ratio<u32>;

/// Shortcut for constructing a [`ReactCell`] for use with a particular environment.
pub fn stateful<Env: HasReact + ?Sized, T>(env: &Env, initial: T) -> ReactCell<Env::React, T> {
    env.react().new_cell(initial)
}

/// Shortcut for constructing a [`ReactCell`] over a dynamic value (one that implements [`Update`])
/// for use with a particular environment.
pub fn stateful_dynamic<Env: HasClock + HasReact + ?Sized, T: Update + Clone>(
    env: &Env,
    initial: T,
) -> Current<ReactCell<Env::React, Trajectory<T>>> {
    env.react()
        .new_cell(Trajectory::new(env.clock(), initial))
        .current()
}