#![feature(generic_associated_types)]
mod drawer;

pub use drawer::*;
use fortify::*;
use uial_core::*;

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

    // Return application
    let size = state.new_cell(vec2(size.width as i32, size.height as i32));
    WgpuApp {
        state,
        window,
        event_loop,
        surface,
        config,
        inner: fortify! {
            let device = device;
            let queue = queue;
            let drawer = WgpuDrawerContext::new(&device, &queue);
            yield WgpuAppInner {
                size: &size,
                device: &device,
                queue: &queue,
                drawer: &drawer
            };
        },
    }
}

/// An interface for setting up and running an application on the WebGPU platform.
struct WgpuApp<S: State + 'static> {
    state: S,
    window: winit::window::Window,
    event_loop: winit::event_loop::EventLoop<()>,
    surface: wgpu::Surface,
    config: wgpu::SurfaceConfiguration,
    inner: Fortify<WgpuAppInner<'static, S>>,
}

/// Encapsulates the borrowed resources for a [`WgpuApp`].
#[derive(Lower)]
struct WgpuAppInner<'a, S: State> {
    size: &'a StateCell<S, Vector2<i32>>,
    device: &'a wgpu::Device,
    queue: &'a wgpu::Queue,
    drawer: &'a WgpuDrawerContext<'a>,
}

impl<S: State + 'static> Application<'static> for WgpuApp<S> {
    type State = S;
    type Drawer = WgpuDrawer<'static>;

    fn state(&mut self) -> &mut Self::State {
        &mut self.state
    }

    fn run(self, widget: impl Widget<Self::State, Self::Drawer> + 'static) -> ! {
        let window = self.window;
        let mut config = self.config;
        let surface = self.surface;
        let runtime = fortify! {
            let inner = self.inner.borrow();
            let elem = widget.place(&mut self.state, inner);
            yield (inner, elem);
        };
        self.event_loop.run(move |event, _, control_flow| {
            *control_flow = winit::event_loop::ControlFlow::Wait;
            match event {
                winit::event::Event::MainEventsCleared => {
                    window.request_redraw();
                }
                winit::event::Event::WindowEvent {
                    event: winit::event::WindowEvent::Resized(size),
                    ..
                } => {
                    config.width = size.width;
                    config.height = size.height;
                    surface.configure(runtime.borrow().0.device, &config);
                }
                winit::event::Event::RedrawRequested(_) => {
                    let frame = surface
                        .get_current_texture()
                        .expect("Failed to acquire next swap chain texture");
                    let view = frame
                        .texture
                        .create_view(&wgpu::TextureViewDescriptor::default());
                    let drawer = runtime.borrow().0.drawer;
                    drawer.with_drawer(&view, (config.width, config.height), |d| {
                        d.fill_rect(
                            Paint::new(255, 0, 0, 255),
                            box2(10, config.width as i32 - 10, 10, config.height as i32 - 10),
                        );
                    });
                    frame.present();
                }
                winit::event::Event::WindowEvent {
                    event: winit::event::WindowEvent::CloseRequested,
                    ..
                } => *control_flow = winit::event_loop::ControlFlow::Exit,
                _ => {}
            }
        })
    }
}
impl<'a, S: State> Placement for &'a WgpuAppInner<'a, S> {
    type State = S;
    fn rect(&self, s: &Self::State) -> Box2<i32> {
        todo!()
    }
}