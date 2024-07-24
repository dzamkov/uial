use uial::*;
use uial::widget::Camera;
use uial::drawer::*;
use uial_backend::*;
use std::rc::Rc;

fn main() {
    const CIRCLES: &[(Vector2, Scalar)] = &[
        (vec2(-17.048965, 22.211098), 29.0),
        (vec2(17.657856, -23.00435), 28.0),
        (vec2(38.66902, 27.824099), 27.0),
        (vec2(-35.957718, -29.436357), 26.0),
        (vec2(-69.42067, 9.050397), 25.0),
        (vec2(8.268666, 68.77307), 24.0),
        (vec2(-5.9794493, -68.19592), 23.0),
        (vec2(66.5375, -12.479101), 22.0),
        (vec2(-36.72737, 68.17587), 21.0),
        (vec2(55.196136, -52.918854), 20.0),
        (vec2(-47.71829, -72.87239), 19.0),
        (vec2(-79.83885, -32.66844), 18.0),
        (vec2(82.41907, 23.140778), 17.0),
        (vec2(48.256943, 69.74154), 16.0),
        (vec2(-67.18761, 48.988014), 15.0),
        (vec2(30.196253, -75.96242), 14.0),
        (vec2(70.85788, 51.570198), 13.0),
        (vec2(84.924866, -41.078133), 12.0),
        (vec2(-16.791138, 93.20678), 11.0),
        (vec2(-73.698326, -59.986824), 10.0),
        (vec2(14.505903, -92.779465), 9.0),
        (vec2(93.477974, 0.71979284), 8.0),
        (vec2(-85.4623, 36.739132), 7.0),
        (vec2(33.067173, 85.65602), 6.0),
        (vec2(-27.610542, -85.975006), 5.0),
        (vec2(-88.76086, -12.558796), 4.0),
        (vec2(47.129543, -74.45789), 3.0),
        (vec2(66.961815, 34.18946), 2.0),
        (vec2(65.24236, 36.647804), 1.0),
    ];
    SimpleApplication {
        title: "Circles",
        body: &|env| {
            let camera = Rc::new(env.react().new_cell(Trajectory::new(
                Instant::ZERO,
                Camera::new(vec2(0.0, 0.0), 5.0, -8.0),
            )));
            widget::zoom_canvas(camera.clone().current(), |_, drawer| {
                for (c, r) in CIRCLES.iter().copied() {
                    drawer.draw_circle(srgb(1.0, 1.0, 0.8).into(), c, r)
                }
            })
            .on_key(move |env, key| {
                match key.key_code {
                    Some(winit::event::VirtualKeyCode::R) => {
                        (&camera).current().set(env, Camera::new(vec2(0.0, 0.0), 5.0, -8.0));
                        true
                    }
                    _ => false
                }
            })
            .into_rc_dyn()
        },
    }
    .run();
}
