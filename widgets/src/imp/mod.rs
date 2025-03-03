pub mod button;
pub mod stretchable;

pub use stretchable::StretchableImage;

use std::rc::Rc;
use uial::drawer::{BorrowImageTTFontFamily, ImageTTFont, ImageTTFontFamily};
use uial::drawer::{HasImageManager, ImageDrawer, ImageHandle, ImageManager};
use uial::prelude::*;

/// Encapsulates the styling information for all common widgets.
pub struct CommonStyle<I: ImageHandle, F: Clone> {
    /// Encapsulates the styling information for buttons.
    pub button: button::TextButtonStyle<I, F>,
}

/// The image handle type used in an environment of the given type at runtime.
pub type RunImageHandle<Env> = Rc<<<Env as HasImageManager>::ImageManager as ImageManager>::Handle>;

/// The font type used in an environment of the given type at runtime.
pub type RunFont<Env> = ImageTTFont<
    <Env as HasImageManager>::ImageManager,
    Rc<ImageTTFontFamily<<Env as HasImageManager>::ImageManager>>,
>;

/// A [`CommonStyle`] for use in an environment of the given type at runtime.
pub type RunCommonStyle<Env> = CommonStyle<RunImageHandle<Env>, RunFont<Env>>;

/// An environment which supports the functionality required to run widgets from a
/// [`RunCommonStyle`].
pub trait RunEnvironment:
    WidgetEnvironment<Drawer = Self::ImageDrawer> + Track + HasImageManager
{
    /// The type of image drawer used by this environment.
    type ImageDrawer: ImageDrawer<<Self::ImageManager as ImageManager>::Source> + ?Sized;
}

impl<Env: WidgetEnvironment + Track + HasImageManager + ?Sized> RunEnvironment for Env
where
    Self::Drawer: ImageDrawer<<Self::ImageManager as ImageManager>::Source>,
{
    type ImageDrawer = Self::Drawer;
}

impl<Env: RunEnvironment + ?Sized> crate::ButtonStyle<Env> for RunCommonStyle<Env> {
    fn button(&self, content: impl IntoWidget<Env>) -> impl crate::IntoButtonWidget<Env> {
        crate::ButtonStyle::button(&self.button.base, content)
    }
}

impl<Env: RunEnvironment + ?Sized> crate::TextButtonStyle<Env> for RunCommonStyle<Env> {
    fn text_button(
        &self,
        text: impl Property<Env, Value = String>,
    ) -> impl crate::IntoButtonWidget<Env> {
        crate::TextButtonStyle::text_button(&self.button, text)
    }
}

/// Loads the default [`RunCommonStyle`] into the given [`WidgetEnvironment`].
pub fn load_default_style<Env: WidgetEnvironment + HasImageManager + ?Sized>(
    env: &Env,
) -> RunCommonStyle<Env> {
    use std::num::NonZeroU32;
    use stretchable::{Band, BandMode, BandPattern};
    use uial::prelude::{Box2i, size2i, vec2i};
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
    CommonStyle {
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
            button::TextButtonStyle {
                base: Rc::new(button::ButtonStyle {
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
                    padding: Padding2i::uniform(8),
                }),
                enabled_font: font
                    .clone()
                    .with_height(20.0)
                    .with_paint(srgba(1.0, 1.0, 1.0, 1.0)),
                disabled_font: font
                    .clone()
                    .with_height(20.0)
                    .with_paint(srgba(0.8, 0.8, 0.8, 1.0)),
            }
        },
    }
}
