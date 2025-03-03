pub mod style;
pub mod imp;

pub use style::*;

pub mod prelude {
    pub use uial::prelude::*;
    pub use crate::style::*;
}

/// Loads the default [`CommonStyle`] into the given [`WidgetEnvironment`].
pub fn load_default_style<Env: imp::RunEnvironment + ?Sized>(
    env: &Env,
) -> impl CommonStyle<Env> + use<Env> {
    imp::load_default_style(env)
}