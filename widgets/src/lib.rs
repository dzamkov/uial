pub mod button;
mod stretchable;
pub mod text_button;

pub use button::*;
use std::rc::Rc;
pub use stretchable::Stretchable;
pub use text_button::*;
use uial::drawer::{BorrowImageTTFontFamily, ImageTTFont, ImageTTFontFamily};
use uial::drawer::{HasImageManager, Image, ImageManager, ImageView};
use uial::{srgba, Padding2i, WidgetEnvironment};

/// Encapsulates the styling information for all available widgets.
pub struct Style<I: Image, F: Clone> {
    /// The style for buttons.
    pub button: Rc<TextButtonStyle<I, F>>,
}

/// A [`Style`] whose resources are managed by an [`ImageManager`] of type `M`.
pub type RunStyle<M> =
    Style<ImageView<Rc<<M as ImageManager>::Image>>, ImageTTFont<M, Rc<ImageTTFontFamily<M>>>>;

/// Loads the default [`Style`] into the given [`WidgetEnvironment`].
pub fn load_default_style<Env: WidgetEnvironment + HasImageManager + ?Sized>(
    env: &Env,
) -> RunStyle<Env::ImageManager> {
    use std::num::NonZeroU32;
    use stretchable::{Band, BandMode, BandPattern};
    use uial::{size2i, vec2i, Box2i};
    let image_manager = env.image_manager();
    let font = Rc::new(
        ImageTTFontFamily::new(
            include_bytes!("default.otb")[..]
                .to_owned()
                .into_boxed_slice(),
            image_manager,
        )
        .unwrap(),
    );
    Style {
        button: {
            let image = image_manager.load_image(
                image::load_from_memory_with_format(
                    include_bytes!("button_default.png"),
                    image::ImageFormat::Png,
                )
                .unwrap(),
            );
            let image = Rc::new(image);
            let pattern = BandPattern::new(
                vec![
                    Band {
                        size: NonZeroU32::new(4).unwrap(),
                        mode: BandMode::Fixed,
                    },
                    Band {
                        size: NonZeroU32::new(1).unwrap(),
                        mode: BandMode::Tiling {
                            weight: NonZeroU32::new(1).unwrap(),
                        },
                    },
                    Band {
                        size: NonZeroU32::new(3).unwrap(),
                        mode: BandMode::Fixed,
                    },
                ],
                vec![
                    Band {
                        size: NonZeroU32::new(3).unwrap(),
                        mode: BandMode::Fixed,
                    },
                    Band {
                        size: NonZeroU32::new(1).unwrap(),
                        mode: BandMode::Tiling {
                            weight: NonZeroU32::new(1).unwrap(),
                        },
                    },
                    Band {
                        size: NonZeroU32::new(1).unwrap(),
                        mode: BandMode::Tiling {
                            weight: NonZeroU32::new(1).unwrap(),
                        },
                    },
                    Band {
                        size: NonZeroU32::new(3).unwrap(),
                        mode: BandMode::Fixed,
                    },
                ],
            )
            .unwrap();
            Rc::new(TextButtonStyle {
                base: Rc::new(ButtonStyle {
                    normal: Stretchable::new(
                        image
                            .clone()
                            .view(Box2i::from_min_size(vec2i(0, 0), size2i(8, 8)))
                            .unwrap(),
                        pattern.clone(),
                    )
                    .unwrap(),
                    hover: Stretchable::new(
                        image
                            .clone()
                            .view(Box2i::from_min_size(vec2i(8, 0), size2i(8, 8)))
                            .unwrap(),
                        pattern.clone(),
                    )
                    .unwrap(),
                    pressed: Stretchable::new(
                        image
                            .clone()
                            .view(Box2i::from_min_size(vec2i(16, 0), size2i(8, 8)))
                            .unwrap(),
                        pattern.clone(),
                    )
                    .unwrap(),
                    disabled: Stretchable::new(
                        image
                            .clone()
                            .view(Box2i::from_min_size(vec2i(24, 0), size2i(8, 8)))
                            .unwrap(),
                        pattern.clone(),
                    )
                    .unwrap(),
                }),
                padding: Padding2i::uniform(8),
                enabled_font: font
                    .clone()
                    .with_height(20.0)
                    .with_paint(srgba(1.0, 1.0, 1.0, 1.0)),
                disabled_font: font
                    .clone()
                    .with_height(20.0)
                    .with_paint(srgba(0.8, 0.8, 0.8, 1.0)),
            })
        },
    }
}
