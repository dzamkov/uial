use uial::*;

fn main() {
    app().run(widget::prepare(|s, g| {
        let img = g.load_image(image::load_from_memory(include_bytes!("ferris.png")).unwrap());
        widget::image(img).with_align(0.5, 0.5)
    }))
}
