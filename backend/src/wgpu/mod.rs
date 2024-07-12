mod erase;
pub mod image;
mod quad;

pub use self::erase::*;
pub use self::image::*;
pub use self::quad::*;
use ::wgpu;
use uial::drawer::*;
use uial::geometry::*;
use bytemuck::{Pod, Zeroable};
use std::{borrow::Cow, ops::Range, rc::Rc};
use wgpu::util::DeviceExt;

/// An "environment" which provides a [`WgpuContext`].
pub trait HasWgpuContext<'wgpu> {
    /// Gets the [`WgpuContext`] for this environment.
    fn wgpu_context(&self) -> &'wgpu WgpuContext;
}

/// Encapsulates a WebGPU context.
pub struct WgpuContext {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
}

/// An "environment" which provides a [`WgpuDrawerContext`].
pub trait HasWgpuDrawerContext<'wgpu> {
    /// Gets the [`WgpuContext`] for this environment.
    fn wgpu_drawer_context(&self) -> &'wgpu WgpuDrawerContext<'wgpu>;
}

/// Contains the context and resources needed to create a [`WgpuDrawer`].
pub struct WgpuDrawerContext<'a> {
    context: &'a WgpuContext,
    draw_format: wgpu::TextureFormat,
    white_image: WgpuImage<'a>,
    bind_group_layout_0: wgpu::BindGroupLayout,
    line_pipeline: wgpu::RenderPipeline,
    tri_pipeline: wgpu::RenderPipeline,
    bind_group_1: wgpu::BindGroup,
}

impl<'a> WgpuDrawerContext<'a> {
    /// Creates a [`WgpuDrawerContext`] for the given [`WgpuContext`].
    pub fn new(
        context: &'a WgpuContext,
        image_atlas: &'a WgpuImageAtlas<'a>,
        draw_format: wgpu::TextureFormat,
    ) -> Self {
        let device = &context.device;

        // Create solid drawing pipelines
        let bind_group_layout_0 =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: wgpu::BufferSize::new(std::mem::size_of::<
                            WgpuDrawerUniforms,
                        >() as u64),
                    },
                    count: None,
                }],
            });
        let bind_group_layout_1 =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::NonFiltering),
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                ],
            });

        // Create image drawing pipeline
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_str!("draw.wgsl"))),
        });
        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&bind_group_layout_0, &bind_group_layout_1],
            push_constant_ranges: &[],
        });
        let create_pipeline = |topology| {
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: None,
                layout: Some(&pipeline_layout),
                vertex: wgpu::VertexState {
                    module: &shader,
                    entry_point: "vs_main",
                    buffers: &[DrawVertex::LAYOUT],
                },
                primitive: wgpu::PrimitiveState {
                    topology,
                    front_face: wgpu::FrontFace::Ccw,
                    cull_mode: Some(wgpu::Face::Back),
                    ..Default::default()
                },
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: WgpuDrawer::DEPTH_STENCIL_FORMAT,
                    depth_write_enabled: true,
                    depth_compare: wgpu::CompareFunction::Greater,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample: wgpu::MultisampleState::default(),
                fragment: Some(wgpu::FragmentState {
                    module: &shader,
                    entry_point: "fs_main",
                    targets: &[Some(wgpu::ColorTargetState {
                        format: draw_format,
                        blend: Some(wgpu::BlendState {
                            color: wgpu::BlendComponent {
                                src_factor: wgpu::BlendFactor::SrcAlpha,
                                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                                operation: wgpu::BlendOperation::Add,
                            },
                            alpha: wgpu::BlendComponent {
                                src_factor: wgpu::BlendFactor::SrcAlpha,
                                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                                operation: wgpu::BlendOperation::Add,
                            },
                        }),
                        write_mask: wgpu::ColorWrites::all(),
                    })],
                }),
                multiview: None,
            })
        };
        let line_pipeline = create_pipeline(wgpu::PrimitiveTopology::LineList);
        let tri_pipeline = create_pipeline(wgpu::PrimitiveTopology::TriangleList);

        // Create bind group
        let image_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: None,
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });
        let image_atlas_view = image_atlas
            .texture
            .create_view(&wgpu::TextureViewDescriptor {
                label: None,
                dimension: Some(wgpu::TextureViewDimension::D2),
                ..wgpu::TextureViewDescriptor::default()
            });
        let bind_group_1 = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &bind_group_layout_1,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::TextureView(&image_atlas_view),
                },
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Sampler(&image_sampler),
                },
            ],
        });

        // Create a solid white image for use with solid-color drawing
        let mut white_image = ::image::RgbaImage::new(1, 1);
        *white_image.get_pixel_mut(0, 0) = [255, 255, 255, 255].into();
        let white_image = image_atlas.load_image(::image::DynamicImage::ImageRgba8(white_image));

        // Finalize context
        Self {
            context,
            draw_format,
            white_image,
            bind_group_layout_0,
            line_pipeline,
            tri_pipeline,
            bind_group_1,
        }
    }

    /// Gets the [`WgpuContext`] this [`WgpuDrawerContext`] is for.
    pub fn context(&self) -> &'a WgpuContext {
        self.context
    }

    /// Gets the [`wgpu::TextureFormat`] required for the target of a [`WgpuDrawer`] constructed
    /// from this [`WgpuDrawerContext`].
    pub fn draw_format(&self) -> wgpu::TextureFormat {
        self.draw_format
    }

    /// Gets the [`wgpu::BindGroupLayout`] for the first bind group of the drawer shader. This
    /// bind group contains a uniforms buffer with [`WgpuDrawerUniforms`] data.
    pub fn bind_group_layout_0(&self) -> &wgpu::BindGroupLayout {
        &self.bind_group_layout_0
    }

    /// Constructs and uses a [`WgpuDrawer`] to draw to a [`wgpu::TextureView`] of the given
    /// size.
    pub fn draw_to<'pass>(
        &self,
        resources: &'pass mut WgpuDrawerResources,
        target: &wgpu::TextureView,
        inner: impl FnOnce(&mut WgpuDrawer<'pass>),
    ) {
        // Get central UV for solid white image
        let white_image = self.white_image.view_all();
        let white_uv = (white_image.rect().min.into_float() + vec2(0.5, 0.5))
            / white_image.store().texture_size as Scalar;
        let white_uv = white_uv.into();

        // Begin render pass
        let vertex_buffer;
        let index_buffer;
        let mut holdings: Vec<Rc<dyn Something + 'pass>> = Vec::new();
        let mut encoder = self
            .context
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        {
            let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: target,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: true,
                    },
                })],
                depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                    view: &resources.depth_stencil_view,
                    depth_ops: Some(wgpu::Operations {
                        load: wgpu::LoadOp::Clear(0.0),
                        store: false,
                    }),
                    stencil_ops: None,
                }),
            });

            // Make common bind group available
            rpass.set_bind_group(0, &resources.bind_group_0, &[]);

            // TODO: Improve justification
            // SAFETY: The command encoder is not accessible from a reference to render pass, so
            // the lifting the lifetime should be okay.
            let rpass: wgpu::RenderPass = unsafe { std::mem::transmute(rpass) };

            // TODO: Stop being lazy and figure out why/whether this is okay
            let holdings = unsafe { std::mem::transmute(&mut holdings) };

            // Do drawing to buffers
            let mut drawer = WgpuDrawer {
                output_size: resources.size,
                rpass,
                holdings,
                next_layer: 1,
                white_uv,
                verts: Vec::new(),
                opaque_lines: Vec::new(),
                opaque_tris: Vec::new(),
                extra_draws: Vec::new(),
            };
            inner(&mut drawer);
            let mut rpass = drawer.rpass;

            // Build geometry buffers
            vertex_buffer =
                self.context
                    .device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(&drawer.verts),
                        usage: wgpu::BufferUsages::VERTEX,
                    });
            let mut draw_start = 0;
            let mut draw_calls = Vec::new();
            let mut indices = drawer.opaque_lines;
            let draw_end = indices.len() as u32;
            if draw_end > draw_start {
                draw_calls.push(DrawCall::Lines(draw_start..draw_end));
                draw_start = draw_end;
            }
            indices.extend(drawer.opaque_tris);
            let draw_end = indices.len() as u32;
            if draw_end > draw_start {
                draw_calls.push(DrawCall::Tris(draw_start..draw_end));
                draw_start = draw_end;
            }
            for draw in drawer.extra_draws {
                match draw {
                    ExtraDraw::Lines(lines) => {
                        indices.extend(lines);
                        let draw_end = indices.len() as u32;
                        if draw_end > draw_start {
                            draw_calls.push(DrawCall::Lines(draw_start..draw_end));
                            draw_start = draw_end;
                        }
                    }
                    ExtraDraw::Tris(tris) => {
                        // TODO: Combine with `opaque_tris` draw call, if possible
                        indices.extend(tris);
                        let draw_end = indices.len() as u32;
                        if draw_end > draw_start {
                            draw_calls.push(DrawCall::Tris(draw_start..draw_end));
                            draw_start = draw_end;
                        }
                    }
                }
            }
            index_buffer =
                self.context
                    .device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(&indices),
                        usage: wgpu::BufferUsages::INDEX,
                    });

            // Render contents of buffers
            rpass.set_bind_group(1, &self.bind_group_1, &[]);
            rpass.set_vertex_buffer(0, vertex_buffer.slice(..));
            rpass.set_index_buffer(index_buffer.slice(..), wgpu::IndexFormat::Uint32);
            for draw_call in draw_calls {
                match draw_call {
                    DrawCall::Lines(indices) => {
                        rpass.set_pipeline(&self.line_pipeline);
                        rpass.draw_indexed(indices, 0, 0..1);
                    }
                    DrawCall::Tris(indices) => {
                        rpass.set_pipeline(&self.tri_pipeline);
                        rpass.draw_indexed(indices, 0, 0..1);
                    }
                }
            }
        }
        self.context.queue.submit(std::iter::once(encoder.finish()));
    }
}

/// Encapsulates the resources required for a single instance of a [`WgpuDrawer`], where the size
/// of the drawing surface is known. These resources may only be used by a single drawer at a time.
pub struct WgpuDrawerResources {
    size: Size2i,
    depth_stencil_view: wgpu::TextureView,
    bind_group_0: wgpu::BindGroup,
}

impl WgpuDrawerResources {
    /// Creates new [`WgpuDrawerResources`] for a draw target of the given size.
    pub fn new(drawer_context: &WgpuDrawerContext, size: Size2i) -> Self {
        let context = drawer_context.context;

        // Create uniforms buffer
        let scale_x = 2.0 / (size.x as f32);
        let scale_y = 2.0 / (size.y as f32);
        let uniforms = WgpuDrawerUniforms {
            proj_view: [
                [scale_x, 0.0, 0.0, 0.0],
                [0.0, scale_y, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [-1.0, -1.0, 0.0, 1.0],
            ],
        };
        let uniforms_buffer =
            context
                .device
                .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: None,
                    usage: wgpu::BufferUsages::UNIFORM,
                    contents: bytemuck::bytes_of(&uniforms),
                });
        let bind_group_0 = context
            .device
            .create_bind_group(&wgpu::BindGroupDescriptor {
                label: None,
                layout: &drawer_context.bind_group_layout_0,
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                        buffer: &uniforms_buffer,
                        offset: 0,
                        size: None,
                    }),
                }],
            });

        // Create depth stencil texture and view
        let depth_stencil_texture = context.device.create_texture(&wgpu::TextureDescriptor {
            label: None,
            size: wgpu::Extent3d {
                width: size.x,
                height: size.y,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: WgpuDrawer::DEPTH_STENCIL_FORMAT,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        });
        let depth_stencil_view =
            depth_stencil_texture.create_view(&wgpu::TextureViewDescriptor::default());

        // Finalize resources
        Self {
            size,
            depth_stencil_view,
            bind_group_0,
        }
    }

    /// The draw target size required to use these resources.
    pub fn size(&self) -> Size2i {
        self.size
    }
    /// Gets the [`wgpu::BindGroup`] for the first bind group of the drawer shader. This
    /// bind group contains a uniforms buffer with [`WgpuDrawerUniforms`] data.
    pub fn bind_group_0(&self) -> &wgpu::BindGroup {
        &self.bind_group_0
    }
}

/// Describes a draw call, assuming geometry data is already available in the bound buffers.
enum DrawCall {
    Lines(Range<u32>),
    Tris(Range<u32>),
}

/// A WebGPU-based interface for drawing onto a two-dimensional surface.
pub struct WgpuDrawer<'pass> {
    output_size: Size2i,
    rpass: wgpu::RenderPass<'pass>,
    holdings: &'pass mut Vec<Rc<dyn Something + 'pass>>,
    next_layer: u32,
    white_uv: [f32; 2],
    verts: Vec<DrawVertex>,
    opaque_lines: Vec<u32>,
    opaque_tris: Vec<u32>,
    extra_draws: Vec<ExtraDraw>,
}

// Placeholder traits for type erasure.
trait Something {}
impl<T> Something for T {}

/// Describes an extra draw call to be performed after the general opaque draw calls. For example,
/// this is used to maintain the ordering of draw calls that contain translucent content.
enum ExtraDraw {
    Lines(Vec<u32>),
    Tris(Vec<u32>),
}

impl<'pass> WgpuDrawer<'pass> {
    /// The [`wgpu::TextureFormat`] used for the depth/stencil attachment.
    pub const DEPTH_STENCIL_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Depth24Plus;

    /// Gets the internal render pass for this [`WgpuDrawer`]. This can be used to interleave
    /// drawing with custom pipelines.
    pub fn render_pass(&mut self) -> &mut wgpu::RenderPass<'pass> {
        &mut self.rpass
    }

    /// Have the [`WgpuDrawer`] hold a strong reference to a value until the render pass is
    /// submitted. This is useful for prolonging the lifetimes of resources passed to
    /// [`WgpuDrawer::render_pass`] in order to satisfy lifetime constraints.
    pub fn hold<T>(&mut self, obj: Rc<T>) -> &'pass T {
        let res = &*obj as *const T;
        self.holdings.push(obj);

        // SAFETY: Since the container of `res` is being held in a vector with a lifetime
        // of `'pass`, `res` should be alive for at least that long.
        unsafe { &*res }
    }

    /// Gets the next layer index for a drawable object. Layers are used with the depth buffer to
    /// ensure that objects occlude each other as if they were drawn in the order they were added,
    /// regardless of what the actual render order is.
    pub fn next_layer(&mut self) -> u32 {
        let layer = self.next_layer;
        self.next_layer += 1;
        layer
    }

    /// Gets the geometry data for a draw call which can be used to draw lines.
    fn verts_lines_mut(&mut self, is_opaque: bool) -> (&mut Vec<DrawVertex>, &mut Vec<u32>) {
        if is_opaque {
            (&mut self.verts, &mut self.opaque_lines)
        } else {
            loop {
                match self.extra_draws.last_mut() {
                    Some(ExtraDraw::Lines(_)) => {
                        // TODO: Rewrite once rust compiler is smart enough to accept it
                        break if let Some(ExtraDraw::Lines(lines)) = self.extra_draws.last_mut() {
                            (&mut self.verts, lines)
                        } else {
                            unreachable!()
                        };
                    }
                    Some(ExtraDraw::Tris(tris)) if tris.is_empty() => {
                        self.extra_draws.pop();
                    }
                    _ => {
                        self.extra_draws.push(ExtraDraw::Lines(Vec::new()));
                    }
                }
            }
        }
    }

    /// Gets the geometry data for a draw call which can be used to draw triangles.
    fn verts_tris_mut(&mut self, is_opaque: bool) -> (&mut Vec<DrawVertex>, &mut Vec<u32>) {
        if is_opaque {
            (&mut self.verts, &mut self.opaque_tris)
        } else {
            loop {
                match self.extra_draws.last_mut() {
                    Some(ExtraDraw::Lines(lines)) if lines.is_empty() => {
                        self.extra_draws.pop();
                    }
                    Some(ExtraDraw::Tris(_)) => {
                        // TODO: Rewrite once rust compiler is smart enough to accept it
                        break if let Some(ExtraDraw::Tris(tris)) = self.extra_draws.last_mut() {
                            (&mut self.verts, tris)
                        } else {
                            unreachable!()
                        };
                    }
                    _ => {
                        self.extra_draws.push(ExtraDraw::Tris(Vec::new()));
                    }
                }
            }
        }
    }
}

impl VectorDrawer for WgpuDrawer<'_> {
    fn visible_bounds(&self) -> Box2 {
        Box2 {
            min: vec2(0.0, 0.0),
            max: vec2(self.output_size.x as Scalar, self.output_size.y as Scalar),
        }
    }

    fn draw_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcDrawer<'_> {
        let layer = self.next_layer();
        let white_uv = self.white_uv;
        let paint: palette::Srgba<u8> = paint.into();
        let (verts, lines) = self.verts_lines_mut(paint.alpha == 255);
        verts.push(DrawVertex {
            pos: start.into(),
            layer,
            uv: white_uv,
            paint,
        });
        let drawer = WgpuPolylineDrawer {
            verts,
            lines,
            layer,
            uv: white_uv,
            paint,
        };
        ArcTessellator::new(drawer, 0.5)
    }

    type PolyarcDrawer<'a> = ArcTessellator<WgpuPolylineDrawer<'a>>
    where
        Self: 'a;

    fn fill_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcFiller<'_> {
        let layer = self.next_layer();
        let white_uv = self.white_uv;
        let paint: palette::Srgba<u8> = paint.into();
        let (verts, tris) = self.verts_tris_mut(paint.alpha == 255);
        let start_index = verts.len() as u32;
        let filler = WgpuPolylineFiller {
            verts,
            tris,
            start_index,
            prev: start,
            layer,
            uv: white_uv,
            paint,
        };
        ArcTessellator::new(filler, 0.5)
    }

    type PolyarcFiller<'a> = ArcTessellator<WgpuPolylineFiller<'a>>
    where
        Self: 'a;
}

impl RasterDrawer for WgpuDrawer<'_> {
    #[allow(clippy::identity_op)]
    fn fill_rect(&mut self, paint: Paint, rect: Box2i) {
        let layer = self.next_layer();
        let white_uv = self.white_uv;
        let paint: palette::Srgba<u8> = paint.into();
        let (verts, tris) = self.verts_tris_mut(paint.alpha == 255);
        let base_index = verts.len() as u32;
        verts.push(DrawVertex {
            pos: vec2i(rect.min.x, rect.min.y).into_float().into(),
            layer,
            uv: white_uv,
            paint,
        });
        verts.push(DrawVertex {
            pos: vec2i(rect.max_exclusive.x, rect.min.y).into_float().into(),
            layer,
            uv: white_uv,
            paint,
        });
        verts.push(DrawVertex {
            pos: vec2i(rect.min.x, rect.max_exclusive.y).into_float().into(),
            layer,
            uv: white_uv,
            paint,
        });
        verts.push(DrawVertex {
            pos: vec2i(rect.max_exclusive.x, rect.max_exclusive.y)
                .into_float()
                .into(),
            layer,
            uv: white_uv,
            paint,
        });
        tris.push(base_index + 0);
        tris.push(base_index + 1);
        tris.push(base_index + 2);
        tris.push(base_index + 2);
        tris.push(base_index + 1);
        tris.push(base_index + 3);
    }
}

impl ImageDrawer<WgpuImageAtlas<'_>> for WgpuDrawer<'_> {
    #[allow(clippy::identity_op)]
    fn draw_image(
        &mut self,
        image: ImageView<WgpuImageAtlas<'_>>,
        paint: Paint,
        trans: Similarity2i,
    ) {
        let layer = self.next_layer();
        let paint: palette::Srgba<u8> = paint.into();
        let (verts, tris) = self.verts_tris_mut(false); // TODO: Handle opaque images
        let base_index = verts.len() as u32;
        let pos_min = trans * vec2i(0, 0);
        let pos_max = trans * image.rect().size().into_vec();
        let uv_scale = 1.0 / image.store().texture_size as Scalar;
        let uv_min = image.rect().min.into_float() * uv_scale;
        let uv_max = uv_min + image.rect().size().into_vec().into_float() * uv_scale;
        verts.push(DrawVertex {
            pos: vec2i(pos_min.x, pos_max.y).into_float().into(),
            layer,
            uv: vec2(uv_min.x, uv_min.y).into(),
            paint,
        });
        verts.push(DrawVertex {
            pos: vec2i(pos_max.x, pos_max.y).into_float().into(),
            layer,
            uv: vec2(uv_max.x, uv_min.y).into(),
            paint,
        });
        verts.push(DrawVertex {
            pos: vec2i(pos_min.x, pos_min.y).into_float().into(),
            layer,
            uv: vec2(uv_min.x, uv_max.y).into(),
            paint,
        });
        verts.push(DrawVertex {
            pos: vec2i(pos_max.x, pos_min.y).into_float().into(),
            layer,
            uv: vec2(uv_max.x, uv_max.y).into(),
            paint,
        });
        tris.push(base_index + 0);
        tris.push(base_index + 2);
        tris.push(base_index + 1);
        tris.push(base_index + 1);
        tris.push(base_index + 2);
        tris.push(base_index + 3);
    }
}

/// A [`PolylineBuilder`] which draws polylines to a [`WgpuDrawer`].
pub struct WgpuPolylineDrawer<'a> {
    verts: &'a mut Vec<DrawVertex>,
    lines: &'a mut Vec<u32>,
    layer: u32,
    uv: [f32; 2],
    paint: palette::Srgba<u8>,
}

impl PolylineBuilder for WgpuPolylineDrawer<'_> {
    fn prev(&self) -> Vector2 {
        self.verts.last().unwrap().pos.into()
    }

    fn line_to(&mut self, point: Vector2) {
        let index = self.verts.len() as u32;
        self.verts.push(DrawVertex {
            pos: point.into(),
            layer: self.layer,
            uv: self.uv,
            paint: self.paint,
        });
        self.lines.push(index - 1);
        self.lines.push(index);
    }
}

/// A [`PolylineBuilder`] which fills and draws a polygon to a [`WgpuDrawer`].
pub struct WgpuPolylineFiller<'a> {
    verts: &'a mut Vec<DrawVertex>,
    tris: &'a mut Vec<u32>,
    start_index: u32,
    prev: Vector2,
    layer: u32,
    uv: [f32; 2],
    paint: palette::Srgba<u8>,
}

impl PolylineBuilder for WgpuPolylineFiller<'_> {
    fn prev(&self) -> Vector2 {
        self.prev
    }

    fn line_to(&mut self, point: Vector2) {
        self.prev = point;
        self.verts.push(DrawVertex {
            pos: point.into(),
            layer: self.layer,
            uv: self.uv,
            paint: self.paint,
        });
    }
}

impl Drop for WgpuPolylineFiller<'_> {
    fn drop(&mut self) {
        // Write triangulation of polygon
        // TODO: Actual polygon triangulation
        let end_index = self.verts.len() as u32;
        for i in (self.start_index + 1)..(end_index - 1) {
            self.tris.push(self.start_index);
            self.tris.push(i);
            self.tris.push(i + 1);
        }
    }
}

/// A type that can be used as a vertex type, with equivalent representation in rust
/// as in WGSL.
pub trait Vertex: Copy + bytemuck::Pod {
    /// The layout for this vertex.
    const LAYOUT: wgpu::VertexBufferLayout<'static>;
}

/// The vertex type used by [`WgpuDrawer`].
#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct DrawVertex {
    pub pos: [f32; 2],
    pub layer: u32,
    pub uv: [f32; 2],
    pub paint: palette::Srgba<u8>,
}

impl Vertex for DrawVertex {
    const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: 24 as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &[
            wgpu::VertexAttribute {
                format: wgpu::VertexFormat::Float32x2,
                offset: 0,
                shader_location: 0,
            },
            wgpu::VertexAttribute {
                format: wgpu::VertexFormat::Uint32,
                offset: 8,
                shader_location: 1,
            },
            wgpu::VertexAttribute {
                format: wgpu::VertexFormat::Float32x2,
                offset: 12,
                shader_location: 2,
            },
            wgpu::VertexAttribute {
                format: wgpu::VertexFormat::Unorm8x4,
                offset: 20,
                shader_location: 3,
            },
        ],
    };
}

/// Encapsulates the uniforms for a [`WgpuDrawer`] shader.
#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
pub struct WgpuDrawerUniforms {
    proj_view: [[f32; 4]; 4],
}
