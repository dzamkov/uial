mod fill;

use crate::*;
pub use fill::*;

pub trait Widget<S: State, D: Drawer> {
    type Inst<Env: WidgetEnvironment<State = S, Drawer = D>>: WidgetInstance<Env = Env>;
    fn size(&self) -> Option<Vector2<u32>>;
    fn inst<Env: WidgetEnvironment<State = S, Drawer = D>>(self, env: Env) -> Self::Inst<Env>;
}

pub trait WidgetEnvironment {
    type State: State;

    type Drawer: Drawer;

    /// Gets the current absolute location (position and size) of the widget.
    fn location(&self, s: &Self::State) -> Box2<i32>;
}

pub trait WidgetInstance {
    /// The type of [`WidgetEnvironment`] used to instantiate the widge.
    type Env: WidgetEnvironment;

    /// Draws the widget to the given [`Drawer`].
    fn draw_to(
        &self,
        s: &<Self::Env as WidgetEnvironment>::State,
        drawer: &mut <Self::Env as WidgetEnvironment>::Drawer,
    );
}

/// A [`WidgetInstance`] constructed directly from a [`SimpleWidget`] and a [`WidgetEnvironment`].
pub struct SimpleWidgetInst<W, Env> {
    source: W,
    env: Env,
}

impl<Env: WidgetEnvironment, W: SimpleWidget<Env::State, Env::Drawer>> SimpleWidgetInst<W, Env> {
    /// Constructs a [`SimpleWidgetInst`] from the given source widget and environment.
    pub fn new(source: W, env: Env) -> Self {
        Self { source, env }
    }
}

pub trait SimpleWidget<S: State, D: Drawer>: Widget<S, D> {
    /// Draws the widget to the given [`Drawer`].
    fn draw_to<Env: WidgetEnvironment<State = S, Drawer = D>>(
        &self,
        env: &Env,
        s: &S,
        drawer: &mut D,
    );
}

impl<Env: WidgetEnvironment, W: SimpleWidget<Env::State, Env::Drawer>> WidgetInstance
    for SimpleWidgetInst<W, Env>
{
    type Env = Env;
    fn draw_to(&self, s: &Env::State, drawer: &mut Env::Drawer) {
        self.source.draw_to(&self.env, s, drawer)
    }
}
