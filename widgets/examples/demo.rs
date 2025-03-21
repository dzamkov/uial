use std::rc::Rc;
use uial_backend::*;
use uial_widgets::prelude::*;

fn main() {
    run(|_| SimpleApp {
        title: "Widgets Demo",
        body: widget::build(|env: &DefaultEnv| {
            let style = uial_widgets::load_default_style(env);
            let is_enabled = Rc::new(env.react().new_cell(true));
            let widget = stack_h![
                text_button(&style, ConstRef("Click Me!"))
                    .set_enabled(is_enabled.clone())
                    .on_click(|_| {
                        println!("Clicked!");
                    }),
                widget::empty().with_width(10),
                text_button(&style, ConstRef("Toggle Enabled")).on_click(move |env| {
                    is_enabled.set(env, !is_enabled.get(env));
                })
            ]
            .minimize()
            .center();
            widget::into_widget(widget, env).into_rc_dyn()
        }),
    })
}
