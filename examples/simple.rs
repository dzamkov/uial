use uial::*;

fn main() {
    app().run(widget::prepare(|s, g| {
        let sizing = s.new_cell(Sizing::exact(vec2(100, 100)));
        widget::fill(Paint::new(255, 128, 0, 255))
            .with_sizing(Box::new(sizing))
            .with_align(0.5, 0.5)
    }))
}
