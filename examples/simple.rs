use uial::*;
use fortify::*;

fn main() {
    app().run(fortify! {
        yield widget::fill(Paint::new(255, 0, 0, 255)).with_size_const(vec2(640, 480));
    })
}
