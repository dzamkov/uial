use super::*;
use ::image;
use ::wgpu;
use std::sync::Mutex;

/// Manages a texture which assembles a set of images of different sizes. Images may be added or
/// removed from the atlas dynamically.
pub struct WgpuImageAtlas<'a> {
    context: &'a WgpuContext,
    pub(super) texture: wgpu::Texture,
    pub(super) texture_size: u32,
    allocator: Mutex<guillotiere::AtlasAllocator>,
}

impl<'a> WgpuImageAtlas<'a> {
    /// Constructs a new [`WgpuImageAtlas`].
    pub fn new(context: &'a WgpuContext) -> Self {
        let texture_size = 4096;
        let texture = context.device.create_texture(&wgpu::TextureDescriptor {
            label: None,
            size: wgpu::Extent3d {
                width: texture_size,
                height: texture_size,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        let allocator = Mutex::new(guillotiere::AtlasAllocator::new(guillotiere::size2(
            texture_size as i32,
            texture_size as i32,
        )));
        Self {
            context,
            texture,
            texture_size,
            allocator,
        }
    }

    /// Gets the [`WgpuContext`] for this [`WgpuImageAtlas`].
    pub fn context(&self) -> &'a WgpuContext {
        self.context
    }
}

impl ImageStore for WgpuImageAtlas<'_> {
    type ImageRect = Box2i;

    fn image_rect_size(&self, view: Box2i) -> Size2i {
        view.size()
    }

    fn image_rect_part(&self, view: Box2i, rect: Box2i) -> Option<Box2i> {
        // TODO: Bounds checking
        Some(Box2i {
            min: view.min + rect.min,
            max_exclusive: view.min + rect.max_exclusive,
        })
    }
}

impl<'a> ImageManager for &'a WgpuImageAtlas<'a> {
    type Store = WgpuImageAtlas<'a>;
    type Image = WgpuImage<'a>;
    fn load_image(&self, source: image::DynamicImage) -> Self::Image {
        let size = guillotiere::size2(source.width() as i32, source.height() as i32);
        let mut allocator = self.allocator.lock().unwrap();
        if let Some(alloc) = allocator.allocate(size) {
            self.context.queue.write_texture(
                wgpu::ImageCopyTexture {
                    texture: &self.texture,
                    mip_level: 0,
                    origin: wgpu::Origin3d {
                        x: alloc.rectangle.min.x as u32,
                        y: alloc.rectangle.min.y as u32,
                        z: 0,
                    },
                    aspect: wgpu::TextureAspect::All,
                },
                source.to_rgba8().as_raw().as_slice(),
                wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: Some(source.width() * 4),
                    rows_per_image: Some(source.height()),
                },
                wgpu::Extent3d {
                    width: source.width(),
                    height: source.height(),
                    depth_or_array_layers: 1,
                },
            );
            WgpuImage { atlas: self, alloc }
        } else {
            todo!()
        }
    }
}

/// An image in a [`WgpuImageAtlas`].
pub struct WgpuImage<'a> {
    atlas: &'a WgpuImageAtlas<'a>,
    alloc: guillotiere::Allocation,
}

impl<'a> AsImageView<WgpuImageAtlas<'a>> for WgpuImage<'a> {
    fn view_all(&self) -> ImageView<WgpuImageAtlas<'a>> {
        let rect = self.alloc.rectangle;
        ImageView::new(
            self.atlas,
            Box2i::from_min_max(vec2i(rect.min.x, rect.min.y), vec2i(rect.max.x, rect.max.y)),
        )
    }
}

impl Drop for WgpuImage<'_> {
    fn drop(&mut self) {
        let mut allocator = self.atlas.allocator.lock().unwrap();
        allocator.deallocate(self.alloc.id);
    }
}
