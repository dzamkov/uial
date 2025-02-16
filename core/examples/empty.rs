use uial::prelude::*;
use uial_backend::*;

fn main() {
    run(|_| SimpleApp {
        title: "Empty example",
        body: &|_| widget::empty().with_size(size2i(200, 200)).into_rc_dyn(),
    })
}
