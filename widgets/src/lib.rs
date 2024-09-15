pub mod button;
mod stretchable;
pub mod text_button;

pub use button::*;
use std::rc::Rc;
pub use stretchable::StretchableImage;
pub use text_button::*;
use uial::drawer::{BorrowImageTTFontFamily, ImageTTFont, ImageTTFontFamily};
use uial::drawer::{HasImageManager, ImageHandle, ImageManager};
use uial::prelude::{srgba, Padding2i, WidgetEnvironment};

/// Encapsulates the styling information for all available widgets.
pub struct Style<I: ImageHandle, F: Clone> {
    /// The style for buttons.
    pub button: Rc<TextButtonStyle<I, F>>,
}

/// A [`Style`] for use in an environment of the given type.
pub type RunStyle<Env> = Style<RunImageHandle<Env>, RunFont<Env>>;

/// The image handle type used in an environment of the given type.
pub type RunImageHandle<Env> = Rc<<<Env as HasImageManager>::ImageManager as ImageManager>::Handle>;

/// The font type used in an environment of the given type.
pub type RunFont<Env> = ImageTTFont<
    <Env as HasImageManager>::ImageManager,
    Rc<ImageTTFontFamily<<Env as HasImageManager>::ImageManager>>,
>;

/// Loads the default [`Style`] into the given [`WidgetEnvironment`].
pub fn load_default_style<Env: WidgetEnvironment + HasImageManager + ?Sized>(
    env: &Env,
) -> RunStyle<Env> {
    use std::num::NonZeroU32;
    use stretchable::{Band, BandMode, BandPattern};
    use uial::prelude::{size2i, vec2i, Box2i};
    let image_manager = env.image_manager();
    let font = Rc::new(
        ImageTTFontFamily::new(
            include_bytes!("default.otb")[..]
                .to_owned()
                .into_boxed_slice(),
            image_manager.clone(),
        )
        .unwrap(),
    );
    Style {
        button: {
            let image = image_manager
                .load_image(
                    image::load_from_memory_with_format(
                        include_bytes!("button_default.png"),
                        image::ImageFormat::Png,
                    )
                    .unwrap(),
                )
                .into_rc();
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
                    normal: StretchableImage::new(
                        image
                            .clone()
                            .view(Box2i::from_min_size(vec2i(0, 0), size2i(8, 8)))
                            .unwrap(),
                        pattern.clone(),
                    )
                    .unwrap(),
                    hover: StretchableImage::new(
                        image
                            .clone()
                            .view(Box2i::from_min_size(vec2i(8, 0), size2i(8, 8)))
                            .unwrap(),
                        pattern.clone(),
                    )
                    .unwrap(),
                    pressed: StretchableImage::new(
                        image
                            .clone()
                            .view(Box2i::from_min_size(vec2i(16, 0), size2i(8, 8)))
                            .unwrap(),
                        pattern.clone(),
                    )
                    .unwrap(),
                    disabled: StretchableImage::new(
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
