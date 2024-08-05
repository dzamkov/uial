#![allow(clippy::bool_assert_comparison)]
mod cache;
pub mod drawer;
mod input;
mod prop;
pub mod react;
mod sizing;
mod time;
mod unique;
pub mod widget;

pub use cache::*;
pub use drawer::{srgb, srgba, Color, Paint};
pub use input::*;
pub use prop::*;
pub use react::{DefaultReact, HasReact, React, ReactCell};
pub use sizing::*;
pub use time::*;
pub use uial_geometry::{size2i, vec2i, Box2i, Padding2i, Point2i, Ortho2i, Size2i, Vector2i};
pub use uial_geometry::{vec2, Box2, Scalar, Vector2, PI};
pub use uial_geometry::{Duration, Instant};

pub use widget::CursorEventResponse;
pub use widget::DynWidget;
pub use widget::{ExtendWidgetExt, MinimizeWidgetExt, PadWidgetExt};
pub use widget::{RestrictWidgetExt, SwitchWidgetExt};
pub use widget::{OnClickWidgetExt, OnKeyWidgetExt};
pub use widget::{Widget, WidgetBase, WidgetEnvironment, WidgetInst, WidgetSlot, WidgetId};

/// Re-exports of the [`uial_geometry`] crate.
pub mod geometry {
    pub use uial_geometry::*;
}

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

/// Gets an opaque implementation of [`ImageManager`] from a suitable environment. By erasing the
/// exact type, this can be used to work around "type annotations required" errors in a closure.
pub fn generic_image_manager<Env: ?Sized + drawer::HasImageManager, Store: drawer::ImageSource>(
    env: &Env,
) -> impl drawer::ImageManager<Source = Store> + Copy
where
    Env::ImageManager: drawer::ImageManager<Source = Store>,
{
    env.image_manager()
}
