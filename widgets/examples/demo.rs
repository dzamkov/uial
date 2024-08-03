use std::rc::Rc;
use uial::*;
use uial_backend::*;

fn main() {
    SimpleApplication {
        title: "Widgets Demo",
        body: &|env| {
            let style = uial_widgets::load_default_style(env);
            let is_enabled = Rc::new(env.react().new_cell(true));
            stack_h![
                uial_widgets::TextButtonBuilder {
                    style: style.button.clone(),
                    is_enabled: is_enabled.clone(),
                    text: const_("Click Me!".to_owned()),
                    on_click: move |env: &mut _| {
                        println!("Clicked!");
                    }
                }
                .build(),
                widget::empty().with_width(10),
                uial_widgets::text_button(
                    style.button.clone(),
                    "Toggle Enabled".to_owned(),
                    move |env| {
                        is_enabled.set(env, !is_enabled.get(env));
                    }
                )
            ]
            .with_width(400)
            .with_height(30)
            .center_h()
            .center_v()
            .into_rc_dyn()
        },
    }
    .run()
}
