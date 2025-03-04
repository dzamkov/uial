use std::rc::Rc;
use uial::drawer::*;
use uial::prelude::*;
use uial_backend::*;

fn main() {
    run(|_| SimpleApp {
        title: "Canvas example",
        body: widget::build(|env: &DefaultEnv| {
            let image_manager = env.image_manager();
            let font_data = include_bytes!("font.ttf")[..].to_owned().into_boxed_slice();
            let font_family = ImageTTFontFamily::new(font_data, image_manager.clone()).unwrap();
            let ferris = image_manager
                .load_image(image::load_from_memory(include_bytes!("ferris.png")).unwrap());
            let num_clicks = Rc::new(env.react().new_cell(0));
            let num_clicks_clone = num_clicks.clone();
            widget::canvas(move |env: &RunEnv<_>, size, d| {
                let center = vec2i(size.x() as i32 / 2, size.y() as i32 / 2);
                d.fill_rect(
                    srgba(1.0, 0.2, 0.2, 0.4),
                    Box2i::from_min_size(center, size2i(300, 200)),
                );
                d.fill_rect(
                    srgba(0.2, 1.0, 0.2, 0.4),
                    Box2i::from_min_size(center + vec2i(50, 50), size2i(300, 200)),
                );
                d.fill_rect(
                    srgba(0.2, 0.2, 1.0, 0.4),
                    Box2i::from_min_size(center + vec2i(100, 100), size2i(300, 200)),
                );
                d.draw_circle(
                    srgba(0.0, 1.0, 0.0, 1.0),
                    size.to_vec().to_float() / 2.0,
                    50.0,
                );
                d.draw_circle(
                    srgba(0.0, 1.0, 0.0, 1.0),
                    size.to_vec().to_float() / 2.0,
                    100.0,
                );
                d.draw_image(
                    ferris.to_source(),
                    Paint::new(1.0, 1.0, 1.0, 1.0),
                    Ortho2i::translate(vec2i(100, 100)),
                );
                d.draw_text_immediate_ltr::<ImageTTFont<_, _>>(
                    &(&font_family)
                        .with_x_height(20.0)
                        .with_paint(srgba(1.0, 1.0, 0.0, 1.0)),
                    vec2i(240, 300),
                    "Howdy! - the rust crab",
                );
                d.draw_text_immediate_ltr::<ImageTTFont<_, _>>(
                    &(&font_family)
                        .with_height(20.0)
                        .with_paint(srgba(1.0, 1.0, 0.0, 1.0)),
                    vec2i(30, 400),
                    &format!("Clock: {:?}", HasClock::clock(env)),
                );
                d.draw_text_immediate_ltr::<ImageTTFont<_, _>>(
                    &(&font_family)
                        .with_height(20.0)
                        .with_paint(srgba(0.0, 1.0, 1.0, 1.0)),
                    vec2i(30, 420),
                    &format!("Clicks: {:?}", num_clicks_clone.get(env)),
                );
                d.draw_text_immediate_ltr::<ImageTTFont<_, _>>(
                    &(&font_family)
                        .with_height(20.0)
                        .with_paint(srgba(1.0, 0.0, 1.0, 1.0)),
                    vec2i(30, 440),
                    &format!("Spacebar is down?: {:?}", env.is_key_down(KeyCode::Space)),
                );
            })
            .on_click(move |env| {
                num_clicks.set(env, num_clicks.get(env) + 1);
            })
        }),
    });
}
