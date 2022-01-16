use crate::*;

pub fn fill<S: State, D: Drawer>(color: Color) -> impl Widget<S, D> {
    FillWidget(color)
}

struct FillWidget(Color);

impl<S: State, D: Drawer> Widget<S, D> for FillWidget {
    type Inst<Env: WidgetEnvironment<State = S, Drawer = D>> = SimpleWidgetInst<Self, Env>;

    fn size(&self) -> Option<Vector2<u32>> {
        None
    }

    fn inst<Env: WidgetEnvironment<State = S, Drawer = D>>(self, env: Env) -> Self::Inst<Env> {
        SimpleWidgetInst::new(self, env)
    }
}

impl<S: State, D: Drawer> SimpleWidget<S, D> for FillWidget {
    fn draw_to<Env: WidgetEnvironment<State = S, Drawer = D>>(
        &self,
        env: &Env,
        s: &S,
        drawer: &mut D,
    ) {
        drawer.fill_rect(Paint::from(self.0), env.location(s))
    }
}