use uial::*;

fn main() {
    app().run(
        widget::fill(Paint::new(255, 128, 0, 255))
            .with_size(vec2(100, 100))
            .with_align(0.5, 0.5),
    )
}
