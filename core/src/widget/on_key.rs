use super::*;

/// Contains [`OnKey`]-related extension methods for [`Widget`].
pub trait OnKeyWidgetExt<Env: WidgetEnvironment + ?Sized>: Widget<Env> + Sized {
    /// Wraps this [`Widget`] to make it focusable and attaches the given key press callback to it.
    fn on_key<F: Fn(&mut Env, Key) -> EventStatus>(self, on_key: F) -> OnKey<Self, F> {
        OnKey::new(self, on_key)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>> OnKeyWidgetExt<Env> for T {}

/// A wrapper over a [`Widget`] which makes it focusable and attaches a key press callback to it.
#[derive(Clone, Copy)]
pub struct OnKey<T, F> {
    inner: T,
    on_key: F,
}

impl<T, F> OnKey<T, F> {
    /// Constructs a new [`OnKey`] wrapper over the given widget with the given callback.
    pub fn new(inner: T, on_key: F) -> Self {
        OnKey { inner, on_key }
    }
}

impl<T: WidgetBase, F> WidgetBase for OnKey<T, F> {
    type Layout = T::Layout;
    fn size(&self, layout: &T::Layout) -> Size2i {
        self.inner.size(layout)
    }
}

impl<T: WidgetBase, F> WidgetInner for OnKey<T, F> {
    type Inner = T;
    fn inner<'a, 'b>(inst: WidgetInst<'a, 'b, Self>) -> WidgetInst<'a, 'b, Self::Inner> {
        WidgetInst {
            widget: &inst.widget.inner,
            min: inst.min,
            layout: inst.layout,
        }
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env>, F: Fn(&mut Env, Key) -> EventStatus>
    Widget<Env> for OnKey<T, F>
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.inner.sizing(env)
    }

    fn layout(&self, env: &Env, size: Size2i) -> <T::Layout as WidgetLayout>::Owned {
        self.inner.layout(env, size)
    }

    fn relayout(&self, layout: &mut T::Layout, env: &Env, size: Size2i) {
        self.inner.relayout(layout, env, size)
    }

    fn outline<'a>(
        inst: WidgetInst<'a, 'a, Self>,
        outliner: &mut (impl WidgetOutliner<'a, Env> + ?Sized),
    ) {
        inst.inner().outline(outliner)
    }

    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer) {
        inst.inner().draw(env, drawer)
    }

    fn hover_interactions<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &Env,
        cursor: impl Cursor<'a, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        inst.inner().hover_interactions(env, cursor, f)
    }

    fn mouse_scroll<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus {
        inst.inner().mouse_scroll(env, cursor, amount)
    }

    fn mouse_down<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus {
        // TODO: Ensure focus is captured at the end of the mouse interaction
        inst.inner().mouse_down(env, cursor, button)
    }

    fn focus<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        keyboard: impl Keyboard<'a, Env> + Clone,
        backward: bool,
    ) -> EventStatus {
        let status = inst.inner().focus(
            env,
            OnKey {
                inner: keyboard.clone(),
                on_key: &inst.widget.on_key,
            },
            backward,
        );
        if let EventStatus::Handled = status {
            return EventStatus::Handled;
        }

        // If the inner widget doesn't handle the focus event, install a keyboard handler just for
        // the `OnKey` widget.
        keyboard.set_handler(
            env,
            DefaultHandler(OnKey {
                inner: keyboard.clone(),
                on_key: &inst.widget.on_key,
            }),
        );
        EventStatus::Handled
    }
}

impl<
        'a,
        Env: WidgetEnvironment + ?Sized,
        T: Keyboard<'a, Env>,
        F: Fn(&mut Env, Key) -> EventStatus,
    > Keyboard<'a, Env> for OnKey<T, &'a F>
{
    fn keys_held(&self, env: &Env, held: impl FnMut(Key)) {
        self.inner.keys_held(env, held)
    }

    fn set_handler(&self, env: &mut Env, handler: impl KeyboardHandler<Env> + 'a) {
        self.inner.set_handler(env, handler);
    }

    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus {
        // Intercept key down events which are handled by the closure
        if let EventStatus::Handled = (self.on_key)(env, key) {
            return EventStatus::Handled;
        }
        self.inner.default_key_down(env, key)
    }
}
