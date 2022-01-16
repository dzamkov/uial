struct DrawImageVertex {
    [[location(0)]] pos: vec2<f32>;
    [[location(1)]] uv: vec2<f32>;
    [[location(2)]] paint: vec4<f32>;
};

struct Inter {
    [[location(0)]] uv: vec2<f32>;
    [[location(1)]] paint: vec4<f32>;
    [[builtin(position)]] pos: vec4<f32>;
};

[[block]]
struct Uniforms {
    proj_view: mat4x4<f32>;
};

[[group(0), binding(0)]]
var<uniform> uniforms: Uniforms;

[[stage(vertex)]]
fn vs_main(in: DrawImageVertex) -> Inter {
    var out: Inter;
    out.uv = in.uv;
    out.paint = in.paint;
    out.pos = uniforms.proj_view * vec4<f32>(in.pos, 0.0, 1.0);
    return out;
}

[[group(1), binding(0)]]
var texture: texture_2d<f32>;

[[group(1), binding(1)]]
var sampler: sampler;

[[stage(fragment)]]
fn fs_main(in: Inter) -> [[location(0)]] vec4<f32> {
    return textureSample(texture, sampler, in.uv) * in.paint;
}