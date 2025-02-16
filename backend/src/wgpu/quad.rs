use wgpu::util::DeviceExt;

/// Encapsulates the resources needed to draw a simple unit quad in the bounds ([0, 1], [0, 1]).
pub struct QuadResources {
    vertex_buffer: wgpu::Buffer,
}

impl QuadResources {
    /// Creates [`QuadResources`] for use with the given device.
    pub fn new(device: &wgpu::Device) -> Self {
        let vertices: [[f32; 2]; 4] = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0], [1.0, 1.0]];
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(&vertices),
            usage: wgpu::BufferUsages::VERTEX,
        });
        Self { vertex_buffer }
    }

    /// The vertex buffer layout for the quad.
    pub const BUFFERS: &'static [wgpu::VertexBufferLayout<'static>] = &[wgpu::VertexBufferLayout {
        array_stride: 8 as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &[wgpu::VertexAttribute {
            format: wgpu::VertexFormat::Float32x2,
            offset: 0,
            shader_location: 0,
        }],
    }];

    /// The primitive topology for the quad.
    pub const PRIMITIVE_TOPOLOGY: wgpu::PrimitiveTopology = wgpu::PrimitiveTopology::TriangleStrip;
}

/// Contains quad-related [`wgpu::RenderPass`] extension methods.
pub trait RenderPassExt {
    /// Draws a unit quad. This automatically binds the appropriate geometry buffers.
    fn draw_quad(&mut self, resources: &QuadResources);
}

impl RenderPassExt for wgpu::RenderPass<'_> {
    fn draw_quad(&mut self, resources: &QuadResources) {
        self.set_vertex_buffer(0, resources.vertex_buffer.slice(..));
        self.draw(0..4, 0..1);
    }
}