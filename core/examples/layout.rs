use uial::*;
use uial_backend::*;

fn main() {
    SimpleApplication {
        title: "Layout example",
        body: &|_| {
            stack_h![
                stack_h![
                    widget::fill(srgb(1.0, 0.0, 0.0).into()),
                    widget::fill(srgb(0.0, 1.0, 0.0).into()),
                    widget::fill(srgb(0.0, 0.0, 1.0).into())
                ]
                .with_size(size2i(200, 200))
                .with_padding(Padding2i::uniform(50)),
                widget::empty(),
                stack_v![
                    widget::fill(srgb(1.0, 0.0, 0.0).into()),
                    widget::fill(srgb(0.0, 1.0, 0.0).into()),
                    widget::fill(srgb(0.0, 0.0, 1.0).into())
                ]
                .with_size(size2i(200, 200))
                .with_padding(Padding2i::uniform(50))
            ]
            .center_v()
            .into_boxed_dyn()
        },
    }
    .run()
}
