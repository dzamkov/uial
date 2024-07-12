struct Uniforms {
    proj_view: mat4x4<f32>,
}

@group(0) @binding(0) var<uniform> uniforms: Uniforms;
@group(1) @binding(0) var image_sampler: sampler;
@group(1) @binding(1) var image_texture: texture_2d<f32>;

struct Vertex {
    @location(0) pos: vec2<f32>,
    @location(1) layer: u32,
    @location(2) uv: vec2<f32>,
    @location(3) paint: vec4<f32>,
};

struct Inter {
    @location(0) uv: vec2<f32>,
    @location(1) paint: vec4<f32>,
    @builtin(position) pos: vec4<f32>,
};

fn linearize_srgb_component(value: f32) -> f32 {
    if (value <= 0.04045) {
        return value / 12.92;
    } else {
        return pow((value + 0.055) / 1.055, 2.4);
    }
}

fn linearize_srgb(color: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(
        linearize_srgb_component(color.r),
        linearize_srgb_component(color.g),
        linearize_srgb_component(color.b)
    );
}

@vertex
fn vs_main(in: Vertex) -> Inter {
    var out: Inter;
    out.uv = in.uv;
    out.paint = vec4<f32>(linearize_srgb(in.paint.rgb), in.paint.a);
    out.pos = uniforms.proj_view * vec4<f32>(in.pos, 0.0, 1.0);
    out.pos.z = f32(in.layer) / 16777216.0;
    return out;
}

@fragment
fn fs_main(in: Inter) -> @location(0) vec4<f32> {
    let res = textureSample(image_texture, image_sampler, in.uv) * in.paint;
    if res.a < 0.01 {
        discard;
    }
    return res;
}