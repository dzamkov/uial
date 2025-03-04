use uial::prelude::*;
use uial_backend::*;

fn main() {
    run(|_| SimpleApp {
        title: "Empty example",
        body: widget::empty().with_size(size2i(200, 200)),
    })
}
