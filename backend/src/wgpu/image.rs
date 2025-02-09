use super::*;
use ::image;
use ::wgpu;
use std::sync::{Arc, Mutex, Weak};

/// Manages a texture which assembles a set of images of different sizes. Images may be added or
/// removed from the atlas dynamically.
///
/// The [`WgpuImageAtlas`] must be stored inside an [`Arc`] so that images allocated from it can
/// maintain a reference to it.
pub struct WgpuImageAtlas {
    context: Arc<WgpuContext>,
    pub(super) texture: wgpu::Texture,
    pub(super) texture_size: u32,
    allocator: Mutex<guillotiere::AtlasAllocator>,
    weak_self: Weak<Self>,
}

impl WgpuImageAtlas {
    /// Constructs a new [`WgpuImageAtlas`] inside of an [`Arc`].
    pub fn new_arc(context: Arc<WgpuContext>) -> Arc<Self> {
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
        Arc::new_cyclic(|weak_self| Self {
            context,
            texture,
            texture_size,
            allocator,
            weak_self: weak_self.clone(),
        })
    }

    /// Gets the [`WgpuContext`] for this [`WgpuImageAtlas`].
    pub fn context(&self) -> &Arc<WgpuContext> {
        &self.context
    }
}

impl ImageHandle for WgpuImageAtlas {
    type Source = WgpuImageAtlas;
    fn source(&self) -> &WgpuImageAtlas {
        self
    }
}

impl ImageSource for WgpuImageAtlas {
    type Rect = Box2i;

    fn image_size(&self, rect: Box2i) -> Size2i {
        rect.size()
    }

    fn image_part(&self, rect: Box2i, sub: Box2i) -> Option<Box2i> {
        // TODO: Bounds checking
        Some(Box2i::from_min_size(
            vec2i(
                rect.min.x + sub.min.x,
                rect.max_exclusive.y - sub.max_exclusive.y,
            ),
            sub.size(),
        ))
    }
}

impl ImageManager for WgpuImageAtlas {
    type Source = WgpuImageAtlas;
    type Handle = WgpuImageHandle;
    fn load_image(&self, source: image::DynamicImage) -> Image<WgpuImageHandle> {
        let size = guillotiere::size2(source.width() as i32, source.height() as i32);
        let mut allocator = self.allocator.lock().unwrap();
        if let Some(alloc) = allocator.allocate(size) {
            self.context.queue.write_texture(
                wgpu::TexelCopyTextureInfo {
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
                wgpu::TexelCopyBufferLayout {
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
            let rect = alloc.rectangle;
            Image::new(
                WgpuImageHandle {
                    atlas: self.weak_self.upgrade().unwrap(),
                    alloc,
                },
                Box2i::from_min_max(vec2i(rect.min.x, rect.min.y), vec2i(rect.max.x, rect.max.y)),
            )
        } else {
            todo!()
        }
    }
}

/// A handle to an allocated image in a [`WgpuImageAtlas`].
pub struct WgpuImageHandle {
    atlas: Arc<WgpuImageAtlas>,
    alloc: guillotiere::Allocation,
}

impl ImageHandle for WgpuImageHandle {
    type Source = WgpuImageAtlas;
    fn source(&self) -> &WgpuImageAtlas {
        &self.atlas
    }
}

impl Drop for WgpuImageHandle {
    fn drop(&mut self) {
        let mut allocator = self.atlas.allocator.lock().unwrap();
        allocator.deallocate(self.alloc.id);
    }
}
