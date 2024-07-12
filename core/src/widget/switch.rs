use super::*;
use std::rc::Rc;

/// Contains [`Switch`]-related extension methods for [`Property`]s.
pub trait SwitchWidgetExt<T: WidgetBase + ?Sized>: PropertyBase<Value = Rc<T>> + Sized {
    /// Constructs a [`Widget`] which reflects the layout and appearance of the [`Widget`] inside
    /// this property.
    fn switch(self) -> Switch<Self, T> {
        Switch(self)
    }
}

impl<P: PropertyBase<Value = Rc<T>>, T: WidgetBase + ?Sized> SwitchWidgetExt<T> for P {}

/// A [`Widget`] defined by a [`Property`] which produces a widget. When the value of the property
/// changes, the widget will completely change its layout and appearance to reflect the new value.
pub struct Switch<P: PropertyBase<Value = Rc<T>>, T: WidgetBase + ?Sized>(P);

/// The layout for a [`Switch`] widget.
pub struct SwitchLayout<T: WidgetBase + ?Sized> {
    inner_widget: Rc<T>,
    inner_layout: <T::Layout as WidgetLayout>::Owned,
}

impl<P: PropertyBase<Value = Rc<T>>, T: WidgetBase + ?Sized> WidgetBase for Switch<P, T> {
    type Layout = SwitchLayout<T>;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        layout.inner_widget.size(layout.inner_layout.borrow())
    }
}

impl<Env: WidgetEnvironment + ?Sized, P: Property<Env, Value = Rc<T>>, T: Widget<Env> + ?Sized>
    Widget<Env> for Switch<P, T>
{
    fn sizing(&self, env: &Env) -> Sizing {
        self.0.with_ref(env, |inner| inner.sizing(env))
    }

    fn layout(&self, env: &Env, size: Size2i) -> <Self::Layout as WidgetLayout>::Owned {
        self.0.with_ref(env, |inner| SwitchLayout {
            inner_widget: inner.clone(),
            inner_layout: inner.layout(env, size),
        })
    }

    fn relayout(&self, layout: &mut Self::Layout, env: &Env, size: Size2i) {
        self.0.with_ref(env, |inner| {
            if std::ptr::eq(inner.as_ref(), layout.inner_widget.as_ref()) {
                inner.relayout(layout.inner_layout.borrow_mut(), env, size)
            } else {
                layout.inner_widget = inner.clone();
                layout.inner_layout = inner.layout(env, size);
            }
        })
    }

    fn outline<'a>(
        inst: WidgetInst<'a, 'a, Self>,
        outliner: &mut (impl WidgetOutliner<'a, Env> + ?Sized),
    ) {
        inner(inst).outline(outliner)
    }

    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer) {
        inner(inst).draw(env, drawer)
    }

    fn hover_interactions<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &Env,
        cursor: impl Cursor<'a, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        // SAFETY: We wrap the cursor in a `SwitchInput` which holds onto a strong reference to
        // the inner widget for as long as it is being used.
        let inner: WidgetInst<'a, 'a, T> = unsafe { std::mem::transmute(inner(inst)) };
        inner.hover_interactions(
            env,
            SwitchInput {
                current: inst.layout.inner_widget.clone(),
                source: cursor,
            },
            f,
        )
    }

    fn mouse_scroll<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus {
        // SAFETY: We wrap the cursor in a `SwitchInput` which holds onto a strong reference to
        // the inner widget for as long as it is being used.
        let inner: WidgetInst<'a, 'a, T> = unsafe { std::mem::transmute(inner(inst)) };
        inner.mouse_scroll(
            env,
            SwitchInput {
                current: inst.layout.inner_widget.clone(),
                source: cursor,
            },
            amount,
        )
    }

    fn mouse_down<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus {
        // SAFETY: We wrap the cursor in a `SwitchInput` which holds onto a strong reference to
        // the inner widget for as long as it is being used.
        let inner: WidgetInst<'a, 'a, T> = unsafe { std::mem::transmute(inner(inst)) };
        inner.mouse_down(
            env,
            SwitchInput {
                current: inst.layout.inner_widget.clone(),
                source: cursor,
            },
            button,
        )
    }

    fn focus<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        keyboard: impl Keyboard<'a, Env> + Clone,
        backward: bool,
    ) -> EventStatus {
        // SAFETY: We wrap the keyboard in a `SwitchInput` which holds onto a strong reference to
        // the inner widget for as long as it is being used.
        let inner: WidgetInst<'a, 'a, T> = unsafe { std::mem::transmute(inner(inst)) };
        inner.focus(
            env,
            SwitchInput {
                current: inst.layout.inner_widget.clone(),
                source: keyboard,
            },
            backward,
        )
    }
}

/// Gets the [`WidgetInst`] for the inner widget of a [`Switch`] widget.
fn inner<'a, P: PropertyBase<Value = Rc<T>>, T: WidgetBase + ?Sized>(
    inst: WidgetInst<'_, 'a, Switch<P, T>>,
) -> WidgetInst<'a, 'a, T> {
    WidgetInst {
        widget: &inst.layout.inner_widget,
        min: inst.min,
        layout: inst.layout.inner_layout.borrow(),
    }
}

/// A wrapper over a [`Cursor`] or [`Keyboard`], given to the inner widget of a [`Switch`] widget.
struct SwitchInput<T: WidgetBase + ?Sized, S> {
    current: Rc<T>,
    source: S,
}

impl<T: WidgetBase + ?Sized, S: Clone> Clone for SwitchInput<T, S> {
    fn clone(&self) -> Self {
        Self {
            current: self.current.clone(),
            source: self.source.clone(),
        }
    }
}

impl<'a, Env: WidgetEnvironment + ?Sized, T: Widget<Env> + ?Sized + 'a, S: Cursor<'a, Env>>
    Cursor<'a, Env> for SwitchInput<T, S>
{
    fn pos(&self, env: &Env) -> Point2i {
        self.source.pos(env)
    }

    type Keyboard = SwitchInput<T, S::Keyboard>;

    fn keyboard(&self, env: &Env) -> Option<Self::Keyboard> {
        self.source.keyboard(env).map(|keyboard| SwitchInput {
            current: self.current.clone(),
            source: keyboard,
        })
    }

    fn set_handler(&self, env: &mut Env, handler: impl CursorHandler<Env> + 'a) {
        self.source.set_handler(
            env,
            SwitchInput {
                current: self.current.clone(),
                source: handler,
            },
        )
    }

    fn clear_handler(&self, env: &mut Env) {
        self.source.clear_handler(env)
    }

    fn default_interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) -> EventStatus {
        self.source.default_interactions(env, f)
    }

    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus {
        self.source.default_mouse_scroll(env, amount)
    }

    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus {
        self.source.default_mouse_down(env, button)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env> + ?Sized, S: CursorHandler<Env>>
    CursorHandler<Env> for SwitchInput<T, S>
{
    fn interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) {
        self.source.interactions(env, f)
    }

    fn mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) {
        self.source.mouse_scroll(env, amount)
    }

    fn mouse_down(&self, env: &mut Env, button: MouseButton) {
        self.source.mouse_down(env, button)
    }

    fn mouse_up(&self, env: &mut Env, button: MouseButton) {
        self.source.mouse_up(env, button)
    }
}

impl<'a, Env: WidgetEnvironment + ?Sized, T: Widget<Env> + ?Sized + 'a, S: Keyboard<'a, Env>>
    Keyboard<'a, Env> for SwitchInput<T, S>
{
    fn keys_held(&self, env: &Env, held: impl FnMut(Key)) {
        self.source.keys_held(env, held)
    }

    fn set_handler(&self, env: &mut Env, handler: impl KeyboardHandler<Env> + 'a) {
        self.source.set_handler(
            env,
            SwitchInput {
                current: self.current.clone(),
                source: handler,
            },
        )
    }

    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus {
        self.source.default_key_down(env, key)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: Widget<Env> + ?Sized, S: KeyboardHandler<Env>>
    KeyboardHandler<Env> for SwitchInput<T, S>
{
    fn key_down(&self, env: &mut Env, key: Key) {
        self.source.key_down(env, key)
    }

    fn key_up(&self, env: &mut Env, key: Key) {
        self.source.key_up(env, key)
    }
}
