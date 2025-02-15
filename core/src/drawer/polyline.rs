use super::*;
use crate::geometry::Rotation2;

/// An interface for constructing a polyline or a polygon by incrementally adding points which
/// are connected to the previous point.
pub trait PolylineBuilder {
    /// Gets the previous point that was added to the [`PolylineBuilder`].
    fn prev(&self) -> Vector2;

    /// Adds a point to the polyline and connects it to the previous point.
    fn line_to(&mut self, point: Vector2);
}

/// An interface for constructing a poly-arc by incrementally adding lines and arcs which are
/// connected to the previous point.
pub trait PolyarcBuilder: PolylineBuilder {
    /// Adds a point to the poly-arc and connects it, using a circular arc with the given relative
    /// sagitta, to the previous point. The sagitta value is signed such that a positive value
    /// will cause the arc to bulge outward towards `(point - self.prev()).cross()`.
    fn circular_arc_to(&mut self, rel_sagitta: Scalar, point: Vector2);
}

impl<T: PolylineBuilder + ?Sized> PolylineBuilder for &mut T {
    fn prev(&self) -> Vector2 {
        (**self).prev()
    }

    fn line_to(&mut self, point: Vector2) {
        (**self).line_to(point)
    }
}

impl<T: PolylineBuilder + ?Sized> PolylineBuilder for Box<T> {
    fn prev(&self) -> Vector2 {
        (**self).prev()
    }

    fn line_to(&mut self, point: Vector2) {
        (**self).line_to(point)
    }
}

impl<T: PolylineBuilder + ?Sized> PolylineBuilder for Transform<Similarity2, T> {
    fn prev(&self) -> Vector2 {
        self.transform.inverse() * self.source.prev()
    }

    fn line_to(&mut self, point: Vector2) {
        self.source.line_to(self.transform * point)
    }
}

impl<T: PolyarcBuilder + ?Sized> PolyarcBuilder for &mut T {
    fn circular_arc_to(&mut self, rel_sagitta: Scalar, point: Vector2) {
        (**self).circular_arc_to(rel_sagitta, point)
    }
}

impl<T: PolyarcBuilder + ?Sized> PolyarcBuilder for Box<T> {
    fn circular_arc_to(&mut self, rel_sagitta: Scalar, point: Vector2) {
        (**self).circular_arc_to(rel_sagitta, point)
    }
}

impl<T: PolyarcBuilder + ?Sized> PolyarcBuilder for Transform<Similarity2, T> {
    fn circular_arc_to(&mut self, rel_sagitta: Scalar, point: Vector2) {
        self.source
            .circular_arc_to(rel_sagitta, self.transform * point)
    }
}

/// A [`PolyarcBuilder`] which tessellates arcs to a given tolerance and then writes them to an
/// underlying [`PolylineBuilder`].
pub struct ArcTessellator<T: PolylineBuilder> {
    tolerance: Scalar,
    pub source: T,
}

impl<T: PolylineBuilder> ArcTessellator<T> {
    /// Constructs a new [`ArcTessellator`].
    pub fn new(source: T, tolerance: Scalar) -> Self {
        assert!(tolerance > 0.0);
        Self { tolerance, source }
    }
}

impl<T: PolylineBuilder> PolylineBuilder for ArcTessellator<T> {
    fn prev(&self) -> Vector2 {
        self.source.prev()
    }

    fn line_to(&mut self, point: Vector2) {
        self.source.line_to(point)
    }
}

impl<T: PolylineBuilder> PolyarcBuilder for ArcTessellator<T> {
    fn circular_arc_to(&mut self, rel_sagitta: Scalar, point: Vector2) {
        let chord = point - self.prev();
        let chord_len = chord.norm();
        let rel_sagitta_abs = rel_sagitta.abs();
        if rel_sagitta_abs * chord_len > self.tolerance {
            let chord_mid = self.prev() + chord * 0.5;
            let rel_radius = rel_sagitta / 2.0 + 1.0 / (8.0 * rel_sagitta);
            let circle_mid = chord_mid - chord.cross() * (rel_sagitta - rel_radius);
            let rel_radius_abs = rel_radius.abs();
            let theta = 2.0 * (1.0 / (2.0 * rel_radius_abs)).min(1.0).asin();
            let theta = if rel_sagitta_abs > rel_radius_abs {
                if rel_sagitta > 0.0 {
                    2.0 * PI - theta
                } else {
                    -2.0 * PI + theta
                }
            } else {
                theta * rel_sagitta.signum()
            };
            build_circular_arc_polyline(
                &mut self.source,
                circle_mid,
                self.tolerance,
                rel_radius_abs * chord_len,
                theta,
            );
        } else {
            self.source.line_to(point)
        }
    }
}

/// Constructs a polyline which approximates the boundary of a circular sector where the circle is
/// centered at the origin. `polyline.prev()` must be a distance of `radius` from the
/// origin.
fn build_circular_arc_polyline(
    polyline: &mut (impl PolylineBuilder + ?Sized),
    offset: Vector2,
    tolerance: Scalar,
    radius: Scalar,
    theta: Scalar,
) {
    // Calculate number of segments required
    let segment_theta = 2.0 * (1.0 - tolerance / radius).acos();
    let num_segments = (theta.abs() / segment_theta).ceil() as usize;
    let segment_theta = theta / (num_segments as Scalar);

    // Add lines for segments
    let rot = Rotation2::from_angle(segment_theta);
    let mut prev = polyline.prev() - offset;
    for _ in 0..num_segments {
        let point = rot * prev;
        polyline.line_to(offset + point);
        prev = point;
    }
}
