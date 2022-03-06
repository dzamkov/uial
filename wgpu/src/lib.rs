#![feature(generic_associated_types)]
mod graphics;

pub use graphics::*;
use fortify::*;
use uial_core::*;
use uial_core::widget::{WidgetInst, Placement, Element};

/// Creates an [`Application`] for the WebGPU platform.
pub fn wgpu_app(state: impl State + 'static) -> impl Application<'static> {
    env_logger::init();

    // Create window
    let event_loop = winit::event_loop::EventLoop::new();
    let window = winit::window::Window::new(&event_loop).unwrap();
    let instance = wgpu::Instance::new(wgpu::Backends::all());
    let surface = unsafe { instance.create_surface(&window) };
    let (device, queue) = pollster::block_on(async {
        // Create adapter
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                force_fallback_adapter: false,
                compatible_surface: Some(&surface),
            })
            .await
            .expect("Failed to find an appropriate adapter");

        // Create device and queue
        return adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features: wgpu::Features::empty(),
                    limits: wgpu::Limits::downlevel_webgl2_defaults()
                        .using_resolution(adapter.limits()),
                },
                None,
            )
            .await
            .expect("Failed to create device");
    });

    // Configure surface
    let size = window.inner_size();
    let config = wgpu::SurfaceConfiguration {
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        format: DRAW_FORMAT,
        width: size.width,
        height: size.height,
        present_mode: wgpu::PresentMode::Mailbox,
    };
    surface.configure(&device, &config);

    // Create graphics context
    let device = &*Box::leak(Box::new(device));
    let queue = &*Box::leak(Box::new(queue));
    let graphics = Box::leak(Box::new(WgpuGraphics::new(&device, &queue))).borrow();

    // Return application
    WgpuApp {
        state,
        event_loop,
        window,
        surface,
        config,
        device,
        queue,
        graphics,
    }
}

/// An interface for setting up and running an application on the WebGPU platform.
struct WgpuApp<S: State> {
    state: S,
    event_loop: winit::event_loop::EventLoop<()>,
    window: winit::window::Window,
    surface: wgpu::Surface,
    config: wgpu::SurfaceConfiguration,
    device: &'static wgpu::Device,
    queue: &'static wgpu::Queue,
    graphics: &'static WgpuGraphics<'static>,
}
/// Encapsulates the runtime resources for a [`WgpuApp`].
#[derive(Lower)]
struct WgpuRuntime<'a, S: State> {
    state: S,
    window: winit::window::Window,
    surface: wgpu::Surface,
    config: wgpu::SurfaceConfiguration,
    size: &'a StateCell<S, Vector2<i32>>,
    device: &'static wgpu::Device,
    queue: &'static wgpu::Queue,
    graphics: &'static WgpuGraphics<'static>,
    elem: &'a dyn Element<WgpuGraphics<'static>, State = S>,
}

/// Describes the placement of a widget in a [`WgpuApp`].
struct WinitPlacement<'a, S: State>(&'a StateCell<S, Vector2<i32>>);

impl<'a, S: State> Placement for WinitPlacement<'a, S> {
    type State = S;
    fn rect(&self, s: &Self::State) -> Box2<i32> {
        let size = s.get_cell(self.0);
        box2(0, size.x, 0, size.y)
    }
}

impl<S: State + 'static> Application<'static> for WgpuApp<S> {
    type State = S;
    type Graphics = WgpuGraphics<'static>;

    fn state(&mut self) -> &mut Self::State {
        &mut self.state
    }

    fn run(self, widget: impl Widget<Self::State, Self::Graphics> + 'static) -> ! {
        run(
            self.event_loop,
            fortify! {
                let mut state = self.state;
                let window = self.window;
                let surface = self.surface;
                let config = self.config;
                let size = state.new_cell(vec2(config.width as i32, config.height as i32));
                let device = self.device;
                let queue = self.queue;
                let graphics = self.graphics;
                let (widget, key) = widget.inst(&mut state, &graphics);
                let elem = widget.place(&mut state, key, WinitPlacement(&size));
                yield WgpuRuntime {
                    state,
                    window,
                    surface,
                    config,
                    size: &size,
                    device: &device,
                    queue: &queue,
                    graphics,
                    elem: &elem
                };
            },
        )
    }
}

fn run<S: State>(
    event_loop: winit::event_loop::EventLoop<()>,
    mut runtime: Fortify<WgpuRuntime<'static, S>>,
) -> ! {
    event_loop.run(move |event, _, control_flow| {
        *control_flow = winit::event_loop::ControlFlow::Wait;
        runtime.with_mut(|runtime| match event {
            winit::event::Event::MainEventsCleared => {
                runtime.window.request_redraw();
            }
            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::Resized(size),
                ..
            } => {
                runtime.config.width = size.width;
                runtime.config.height = size.height;
                runtime.surface.configure(runtime.device, &runtime.config);
                runtime.state.set_cell(runtime.size, vec2(size.width as i32, size.height as i32));
            }
            winit::event::Event::RedrawRequested(_) => {
                let frame = runtime
                    .surface
                    .get_current_texture()
                    .expect("Failed to acquire next swap chain texture");
                let view = frame
                    .texture
                    .create_view(&wgpu::TextureViewDescriptor::default());
                let width = runtime.config.width;
                let height = runtime.config.height;
                runtime.graphics.with_drawer(&view, (width, height), |d| {
                    runtime.elem.draw_to(&runtime.state, d);
                });
                frame.present();
            }
            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::CloseRequested,
                ..
            } => *control_flow = winit::event_loop::ControlFlow::Exit,
            _ => {}
        })
    })
}
