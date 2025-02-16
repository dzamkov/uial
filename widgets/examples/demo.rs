use std::rc::Rc;
use uial::prelude::*;
use uial_backend::*;

fn main() {
    run(|_| SimpleApp {
        title: "Widgets Demo",
        body: &|env| {
            let style = uial_widgets::load_default_style(env);
            let is_enabled = Rc::new(env.react().new_cell(true));
            stack_h![
                uial_widgets::TextButtonBuilder {
                    style: style.button.clone(),
                    is_enabled: DynProperty::from_rc(is_enabled.clone()).into(),
                    text: const_("Click Me!".to_owned()).into(),
                    on_click: Box::new(move |env: &mut _| {
                        println!("Clicked!");
                    })
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
            .minimize()
            .center()
            .into_rc_dyn()
        },
    })
}
