use uial::prelude::*;
use uial_backend::*;

fn main() {
    SimpleApplication {
        title: "Layout example",
        body: &|_| {
            overlay![
                stack_h![
                    widget::fill(srgb(1.0, 0.0, 0.0).into()),
                    widget::fill(srgb(0.0, 1.0, 0.0).into()),
                    widget::fill(srgb(0.0, 0.0, 1.0).into())
                ]
                .with_size(size2i(200, 200))
                .with_padding(Padding2i::uniform(50))
                .extend_h(0.into()),
                stack_v![
                    widget::fill(srgb(1.0, 0.0, 0.0).into()),
                    widget::fill(srgb(0.0, 1.0, 0.0).into()),
                    widget::fill(srgb(0.0, 0.0, 1.0).into())
                ]
                .with_size(size2i(200, 200))
                .with_padding(Padding2i::uniform(50))
                .extend_h(1.into())
            ]
            .center_v()
            .into_rc_dyn()
        },
    }
    .run()
}
