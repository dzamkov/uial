use crate::drawer::Transform;
use crate::prelude::*;
use std::any::Any;
use std::marker::PhantomData;
use uial_geometry::Similarity2;
use winit::event::MouseScrollDelta;

/// Encapsulates the state information for the "camera" of a [`ZoomCanvas`] which determines the
/// projection used to draw the canvas contents.
#[derive(Debug, Clone, Copy)]
pub struct Camera {
    /// The current surface position at the center of the view.
    pub center: Vector2,

    /// The current rate of change of `center` relative to the *inverse* scale of the camera.
    pub rel_d_center: Vector2,

    /// The current scaling factor applied by the camera.
    pub scale: Scalar,

    /// The current rate of change of the natural logarithm of `scale`.
    pub d_ln_scale: Scalar,

    /// The natural logarithm of the damping factor for the camera's motion. This factor describes
    /// the instantaneous rate of change of `rel_d_center` and `d_ln_scale` relative to their
    /// current values. Since the damping factor must be no greater than 1, this field should be
    /// non-positive.
    pub ln_damping: Scalar,
}

impl Camera {
    /// Constructs a new initially-motionless [`Camera`].
    pub fn new(center: Vector2, scale: Scalar, ln_damping: Scalar) -> Self {
        Self {
            center,
            rel_d_center: vec2(0.0, 0.0),
            scale,
            d_ln_scale: 0.0,
            ln_damping,
        }
    }

    /// Gets the projection from surface coordinates to view coordinates (pixel-sized, with the
    /// origin at the center of the viewport).
    pub fn projection(&self) -> Similarity2 {
        Similarity2::scale(self.scale) * Similarity2::translate(-self.center)
    }

    /// Projects the given point from surface coordinates to view coordinates.
    pub fn proj(&self, point: Vector2) -> Vector2 {
        (point - self.center) * self.scale
    }

    /// Unprojects the given point from view coordinates to surface coordinates.
    pub fn unproj(&self, point: Vector2) -> Vector2 {
        point / self.scale + self.center
    }

    /// Zooms into or away from a target point in surface space. `ln_scale` is the total change in
    /// the natural logarithm of the scaling factor for the camera.
    pub fn zoom(&mut self, target: Vector2, ln_scale: Scalar) {
        let d_ln_scale = ln_scale * -self.ln_damping;
        let offset = target - self.center;
        let rel_d_center = offset * (self.scale * d_ln_scale);
        self.d_ln_scale += d_ln_scale;
        self.rel_d_center += rel_d_center;
    }
}

impl Update for Camera {
    fn update(&mut self, d_time: Duration) {
        let d_time = d_time.as_secs_scalar();
        let damping = (self.ln_damping * d_time).exp();
        let eff_time = (damping - 1.0) / self.ln_damping;
        let init_inv_scale = 1.0 / self.scale;
        self.scale *= (self.d_ln_scale * eff_time).exp();
        if self.d_ln_scale.is_normal() {
            let new_inv_scale = 1.0 / self.scale;
            self.center += self.rel_d_center * (init_inv_scale - new_inv_scale) / self.d_ln_scale;
        } else {
            // `d_ln_scale` is small enough that we don't have to consider changes in scale as
            // we integrate `rel_d_center`.
            self.center += self.rel_d_center * init_inv_scale * eff_time;
        }
        self.d_ln_scale *= damping;
        self.rel_d_center *= damping;
    }
}

#[test]
fn test_camera_update() {
    use approx::assert_relative_eq;
    let init = Camera {
        center: vec2(2.0, -3.0),
        rel_d_center: vec2(10.0, 6.0),
        scale: 50.0,
        d_ln_scale: -0.1,
        ln_damping: 0.9f32.ln(),
    };
    let d2 = Duration::from_secs(2);
    let d3 = Duration::from_secs(3);
    let d5 = d2 + d3;
    let mut a = init;
    a.update(d2);
    a.update(d3);
    let mut b = init;
    b.update(d5);
    let max_relative = 0.0001;
    assert_relative_eq!(a.center, b.center, max_relative = max_relative);
    assert_relative_eq!(a.rel_d_center, b.rel_d_center, max_relative = max_relative);
    assert_relative_eq!(a.scale, b.scale, max_relative = max_relative);
    assert_relative_eq!(a.d_ln_scale, b.d_ln_scale, max_relative = max_relative);
}

/// A widget which presents an pannable, zoomable view of a continuous two-dimensional surface
/// which is drawn using a custom closure.
pub struct ZoomCanvas<
    Env: WidgetEnvironment + ?Sized,
    C: PropertyBase<Value = Camera>,
    F: Fn(&Env, &mut Transform<Similarity2, &mut Env::Drawer>),
> {
    _marker: PhantomData<fn(&Env)>,
    camera: C,
    draw: F,
}

impl<
        Env: WidgetEnvironment + ?Sized,
        C: Field<Env, Value = Camera>,
        F: Fn(&Env, &mut Transform<Similarity2, &mut Env::Drawer>),
    > ZoomCanvas<Env, C, F>
{
    /// Constructs a new [`ZoomCanvas`] widget.
    pub fn new(camera: C, draw: F) -> Self {
        Self {
            _marker: PhantomData,
            camera,
            draw,
        }
    }
}

/// Constructs a widget which presents an pannable, zoomable view of a continuous two-dimensional
/// surface which is drawn using the given closure.
pub fn zoom_canvas<
    Env: WidgetEnvironment + ?Sized,
    C: Field<Env, Value = Camera>,
    F: Fn(&Env, &mut Transform<Similarity2, &mut Env::Drawer>),
>(
    camera: C,
    draw: F,
) -> ZoomCanvas<Env, C, F> {
    ZoomCanvas::new(camera, draw)
}

impl<
        Env: WidgetEnvironment + ?Sized,
        C: PropertyBase<Value = Camera>,
        F: Fn(&Env, &mut Transform<Similarity2, &mut Env::Drawer>),
    > WidgetBase for ZoomCanvas<Env, C, F>
{
}

impl<
        Env: WidgetEnvironment + ?Sized,
        C: Field<Env, Value = Camera>,
        F: Fn(&Env, &mut Transform<Similarity2, &mut Env::Drawer>),
    > Widget<Env> for ZoomCanvas<Env, C, F>
{
    fn sizing(&self, _: &Env) -> Sizing {
        Sizing::any()
    }

    fn inst<'a, S: WidgetSlot<Env> + 'a>(&'a self, _: &Env, slot: S) -> impl WidgetInst<Env> + 'a
    where
        Env: 'a,
    {
        ZoomCanvasInst { widget: self, slot }
    }
}

/// An instance of a [`ZoomCanvas`] widget.
pub struct ZoomCanvasInst<
    'a,
    Env: WidgetEnvironment + ?Sized,
    C: PropertyBase<Value = Camera>,
    F: Fn(&Env, &mut Transform<Similarity2, &mut Env::Drawer>),
    Slot,
> {
    widget: &'a ZoomCanvas<Env, C, F>,
    slot: Slot,
}

impl<
        Env: WidgetEnvironment + ?Sized,
        C: Field<Env, Value = Camera>,
        F: Fn(&Env, &mut Transform<Similarity2, &mut Env::Drawer>),
        Slot: WidgetSlot<Env>,
    > WidgetInst<Env> for ZoomCanvasInst<'_, Env, C, F, Slot>
{
    fn draw(&self, env: &Env, drawer: &mut Env::Drawer) {
        let mut proj = self.widget.camera.get(env).projection();
        let bounds = self.slot.bounds(env);
        proj = Similarity2::translate(
            bounds.min.into_float() + bounds.size().into_vec().into_float() / 2.0,
        ) * proj;
        (self.widget.draw)(env, &mut Transform::new(drawer, proj))
    }

    fn hover_feedback(&self, _: &Env, _: Vector2i, _: &mut dyn FnMut(&dyn Any)) -> bool {
        false
    }

    fn cursor_event(
        &self,
        env: &mut Env,
        pos: Vector2i,
        event: CursorEvent,
    ) -> CursorEventResponse<Env> {
        match event {
            CursorEvent::MouseScroll(amount) => {
                let ln_scale = match amount {
                    MouseScrollDelta::LineDelta(_, y) => y * 0.3,
                    MouseScrollDelta::PixelDelta(delta) => (delta.y as f32) * 0.01,
                };
                let bounds = self.slot.bounds(env);
                self.widget.camera.with_mut(env, |camera| {
                    let point = camera.unproj(
                        (pos - bounds.min).into_float()
                            - bounds.size().into_vec().into_float() / 2.0,
                    );
                    camera.zoom(point, ln_scale);
                });
                CursorEventResponse::Handled
            }
            _ => CursorEventResponse::Bubble,
        }
    }

    fn focus(&self, _: &mut Env, _: bool) -> Option<FocusInteractionRequest<Env>> {
        None
    }
}
