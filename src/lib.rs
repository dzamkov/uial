pub use uial_core::*;

/// Creates an [`Application`].
pub fn app() -> impl Application<'static> {
    let state = unsafe { ClockState::new_unchecked() };
    uial_wgpu::wgpu_app(state)
}