use crate::*;
use image::GenericImageView;
use std::{cell::Cell, num::NonZeroU32};
use wgpu::util::*;

/// The preferred format for drawing.
pub static DRAW_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Bgra8UnormSrgb;

/// Encapsulates the resources needed to create and use a [`WgpuDrawer`].
#[derive(Lower)]
pub struct WgpuGraphics<'a> {
    device: &'a wgpu::Device,
    queue: &'a wgpu::Queue,
    image_atlas: &'a WgpuTextureAtlas<'a>,
    bind_group_layout_0: wgpu::BindGroupLayout,
    draw_image_pipeline: wgpu::RenderPipeline,
    draw_image_bind_group_layout_1: wgpu::BindGroupLayout,
}

impl<'a> WgpuGraphics<'a> {
    /// Creates a new [`WgpuGraphics`] for the given device and queue.
    pub fn new(device: &'a wgpu::Device, queue: &'a wgpu::Queue) -> Fortify<WgpuGraphics<'a>> {
        // Create uniforms bind group layout
        let bind_group_layout_0 =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None, // TODO
                    },
                    count: None,
                }],
            });

        // Create pipeline
        let draw_image_shader = device.create_shader_module(&wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Wgsl(std::borrow::Cow::Borrowed(include_str!(
                "draw_image.wgsl"
            ))),
        });
        let draw_image_bind_group_layout_1 =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler {
                            filtering: true,
                            comparison: false,
                        },
                        count: None,
                    },
                ],
            });
        let draw_image_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: None,
                bind_group_layouts: &[&bind_group_layout_0, &draw_image_bind_group_layout_1],
                push_constant_ranges: &[],
            });
        let draw_image_vertex_layout = wgpu::VertexBufferLayout {
            array_stride: 20 as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 0,
                    shader_location: 0,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x2,
                    offset: 8,
                    shader_location: 1,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Unorm8x4,
                    offset: 16,
                    shader_location: 2,
                },
            ],
        };
        let draw_image_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: None,
            layout: Some(&draw_image_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &draw_image_shader,
                entry_point: "vs_main",
                buffers: &[draw_image_vertex_layout],
            },
            fragment: Some(wgpu::FragmentState {
                module: &draw_image_shader,
                entry_point: "fs_main",
                targets: &[DRAW_FORMAT.into()],
            }),
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
        });

        fortify! {
            // Create texture atlas
            let image_atlas = WgpuTextureAtlas::new(device, queue, 1024);

            // Build graphics context
            yield WgpuGraphics {
                device,
                queue,
                image_atlas: &image_atlas,
                bind_group_layout_0,
                draw_image_pipeline,
                draw_image_bind_group_layout_1,
            };
        }
    }

    /// Draws onto the given [`wgpu::TextureView`] using a [`WgpuDrawer`].
    pub fn with_drawer(
        &self,
        view: &wgpu::TextureView,
        size: (u32, u32),
        draw: impl FnOnce(&mut WgpuDrawer<'a>),
    ) {
        // Create uniform buffer
        let scale_x = 2.0 / (size.0 as f32);
        let scale_y = 2.0 / (size.1 as f32);
        let uniforms = DrawUniforms {
            proj_view: [
                [scale_x, 0.0, 0.0, 0.0],
                [0.0, scale_y, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [-1.0, -1.0, 0.0, 1.0],
            ],
        };
        let uniforms_buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: bytemuck::bytes_of(&uniforms),
                usage: wgpu::BufferUsages::UNIFORM,
            });
        let bind_group_0 = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &self.bind_group_layout_0,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: uniforms_buffer.as_entire_binding(),
            }],
        });

        // Create texture bind group
        let image_texture_view =
            self.image_atlas
                .texture
                .create_view(&wgpu::TextureViewDescriptor {
                    label: None,
                    dimension: Some(wgpu::TextureViewDimension::D2),
                    ..wgpu::TextureViewDescriptor::default()
                });
        let sampler = self.device.create_sampler(&wgpu::SamplerDescriptor {
            label: None,
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });
        let draw_image_bind_group_1 = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &self.draw_image_bind_group_layout_1,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&image_texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });

        // Build draw data
        let mut drawer = WgpuDrawer {
            image_atlas: self.image_atlas,
            draw_image_verts: Vec::new(),
            draw_image_indices: Vec::new(),
        };
        draw(&mut drawer);

        // Build draw buffer
        let draw_image_vertex_buffer = wgpu::util::DeviceExt::create_buffer_init(
            self.device,
            &wgpu::util::BufferInitDescriptor {
                label: None,
                contents: bytemuck::cast_slice(drawer.draw_image_verts.as_slice()),
                usage: wgpu::BufferUsages::VERTEX,
            },
        );
        let draw_image_index_count = drawer.draw_image_indices.len() as u32;
        let draw_image_index_format = wgpu::IndexFormat::Uint32;
        let draw_image_index_buffer = wgpu::util::DeviceExt::create_buffer_init(
            self.device,
            &wgpu::util::BufferInitDescriptor {
                label: None,
                contents: bytemuck::cast_slice(drawer.draw_image_indices.as_slice()),
                usage: wgpu::BufferUsages::INDEX,
            },
        );

        // Encode draw commands
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        {
            let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[wgpu::RenderPassColorAttachment {
                    view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: true,
                    },
                }],
                depth_stencil_attachment: None,
            });
            rpass.set_pipeline(&self.draw_image_pipeline);
            rpass.set_bind_group(0, &bind_group_0, &[]);
            rpass.set_bind_group(1, &draw_image_bind_group_1, &[]);
            rpass.set_index_buffer(draw_image_index_buffer.slice(..), draw_image_index_format);
            rpass.set_vertex_buffer(0, draw_image_vertex_buffer.slice(..));
            rpass.draw_indexed(0..draw_image_index_count, 0, 0..1);
        }
        self.queue.submit(Some(encoder.finish()));
    }
}

/// Manages a texture which assembles a set of images of different sizes. Images may be added or
/// removed from the atlas dynamically.
struct WgpuTextureAtlas<'a> {
    device: &'a wgpu::Device,
    queue: &'a wgpu::Queue,
    texture: wgpu::Texture,
    texture_size: u32,
    allocator: Cell<guillotiere::AtlasAllocator>,
}

impl<'a> WgpuTextureAtlas<'a> {
    /// Constructs a [`WgpuTextureAtlas`] with the given initial capacity.
    pub fn new(device: &'a wgpu::Device, queue: &'a wgpu::Queue, capacity: u32) -> Self {
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: None,
            size: wgpu::Extent3d {
                width: capacity,
                height: capacity,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        });
        let allocator = Cell::new(guillotiere::AtlasAllocator::new(guillotiere::size2(
            capacity as i32,
            capacity as i32,
        )));
        Self {
            device,
            queue,
            texture,
            texture_size: capacity,
            allocator,
        }
    }

    /// Allocates and loads an image into this texture atlas.
    pub fn alloc(&'a self, image: image::DynamicImage) -> WgpuImage<'a> {
        let size = guillotiere::size2(image.width() as i32, image.height() as i32);
        if let Some(alloc) = unsafe { (*self.allocator.as_ptr()).allocate(size) } {
            self.queue.write_texture(
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
                image.to_rgba8().as_raw().as_slice(),
                wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: Some(NonZeroU32::new(image.width() as u32 * 4).unwrap()),
                    rows_per_image: Some(NonZeroU32::new(image.height() as u32).unwrap()),
                },
                wgpu::Extent3d {
                    width: image.width(),
                    height: image.height(),
                    depth_or_array_layers: 1,
                },
            );
            WgpuImage { atlas: self, alloc }
        } else {
            todo!()
        }
    }
}

impl<'a> Graphics for WgpuGraphics<'a> {
    type Image = WgpuImage<'a>;
    type Drawer<'b> = WgpuDrawer<'a>;
    fn load_image(&self, source: image::DynamicImage) -> Image<Self> {
        Image::new(self.image_atlas.alloc(source))
    }
}

/// A [`Drawer`] implemented using `wgpu`.
pub struct WgpuDrawer<'a> {
    image_atlas: &'a WgpuTextureAtlas<'a>,
    draw_image_verts: Vec<DrawImageVertex>,
    draw_image_indices: Vec<u32>,
}

impl<'a> Drawer for WgpuDrawer<'a> {
    type Graphics = WgpuGraphics<'a>;

    fn context(&self) -> &Self::Graphics {
        todo!()
    }

    fn draw_image(
        &mut self,
        image: &Image<WgpuGraphics<'a>>,
        paint: Paint,
        src: Box2<i32>,
        trans: GridAffine2<i32>,
    ) {
        let alloc_rect = image.alloc.rectangle;
        let src = box2(
            alloc_rect.min.x + src.min.x,
            alloc_rect.min.x + src.max.x,
            alloc_rect.min.y + src.min.y,
            alloc_rect.min.y + src.max.y,
        );
        let size_x = src.max.x - src.min.x;
        let size_y = src.max.y - src.min.y;
        let uv_scale = 1.0 / self.image_atlas.texture_size as f32;
        fn approx_into(pt: Vector2<i32>) -> Vector2<f32> {
            vec2(pt.x as f32, pt.y as f32)
        }
        let base_index = self.draw_image_verts.len() as u32;
        let paint = [paint.r(), paint.g(), paint.b(), paint.a()];
        self.draw_image_verts.push(DrawImageVertex {
            pos: approx_into(trans * vec2(0, size_y)).into(),
            uv: (approx_into(vec2(src.min.x, src.min.y)) * uv_scale).into(),
            paint,
        });
        self.draw_image_verts.push(DrawImageVertex {
            pos: approx_into(trans * vec2(size_x, size_y)).into(),
            uv: (approx_into(vec2(src.max.x, src.min.y)) * uv_scale).into(),
            paint,
        });
        self.draw_image_verts.push(DrawImageVertex {
            pos: approx_into(trans * vec2(0, 0)).into(),
            uv: (approx_into(vec2(src.min.x, src.max.y)) * uv_scale).into(),
            paint,
        });
        self.draw_image_verts.push(DrawImageVertex {
            pos: approx_into(trans * vec2(size_x, 0)).into(),
            uv: (approx_into(vec2(src.max.x, src.max.y)) * uv_scale).into(),
            paint,
        });
        self.draw_image_indices.push(base_index + 0);
        self.draw_image_indices.push(base_index + 2);
        self.draw_image_indices.push(base_index + 1);
        self.draw_image_indices.push(base_index + 1);
        self.draw_image_indices.push(base_index + 2);
        self.draw_image_indices.push(base_index + 3);
    }
}

/// An image that can be used by a [`WgpuDrawer`].
pub struct WgpuImage<'a> {
    atlas: &'a WgpuTextureAtlas<'a>,
    alloc: guillotiere::Allocation,
}

impl<'a> Clone for WgpuImage<'a> {
    fn clone(&self) -> Self {
        todo!()
    }
}

impl<'a> ImageSize for WgpuImage<'a> {
    fn size(&self) -> Vector2<u32> {
        vec2(self.alloc.rectangle.width() as u32, self.alloc.rectangle.height() as u32)
    }
}

impl<'a> Drop for WgpuImage<'a> {
    fn drop(&mut self) {
        unsafe { (*self.atlas.allocator.as_ptr()).deallocate(self.alloc.id) }
    }
}

/// The uniforms common to all draw operations.
#[repr(C)]
#[derive(Clone, Copy, bytemuck::Pod, bytemuck::Zeroable, Debug)]
struct DrawUniforms {
    proj_view: [[f32; 4]; 4],
}

/// The vertex type used for a [`Drawer::draw_image`] operation.
#[repr(C)]
#[derive(Clone, Copy, bytemuck::Pod, bytemuck::Zeroable, Debug)]
struct DrawImageVertex {
    pub pos: [f32; 2],
    pub uv: [f32; 2],
    pub paint: [u8; 4],
}
