use super::*;
use std::rc::Rc;

/// A [`Widget`] of an unspecified type, determined at runtime.
#[repr(transparent)]
pub struct DynWidget<'a, Env: WidgetEnvironment + ?Sized>(dyn WidgetSafe<Env> + 'a);

impl<'a, Env: WidgetEnvironment + ?Sized> DynWidget<'a, Env> {
    /// Converts a reference to a [`Widget`] into a reference to a [`DynWidget`].
    pub fn from_ref<W: Widget<Env> + 'a>(source: &W) -> &Self {
        let source: &(dyn WidgetSafe<Env> + 'a) = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts a boxed [`Widget`] into a boxed [`DynWidget`].
    pub fn from_box<W: Widget<Env> + 'a>(source: Box<W>) -> Box<Self> {
        let source: Box<dyn WidgetSafe<Env> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts an [`Rc`]-wrapped [`Widget`] into a boxed [`DynWidget`].
    pub fn from_rc<W: Widget<Env> + 'a>(source: Rc<W>) -> Rc<Self> {
        let source: Rc<dyn WidgetSafe<Env> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }
}

/// An object-safe variant of [`Widget`].
trait WidgetSafe<Env: WidgetEnvironment + ?Sized> {
    fn size(&self, layout: &DynWidgetLayout<Env>) -> Size2i;
    fn sizing(&self, env: &Env) -> Sizing;
    unsafe fn layout(&self, env: &Env, size: Size2i) -> Box<DynWidgetLayout<'static, Env>>;
    fn relayout<'a>(&self, layout: &mut DynWidgetLayout<'a, Env>, env: &Env, size: Size2i)
    where
        Self: 'a;

    fn outline<'a>(
        &'a self,
        min: Vector2i,
        layout: &'a DynWidgetLayout<Env>,
        outliner: &mut dyn WidgetOutliner<'a, Env>,
    );

    fn draw(
        &self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &Env,
        drawer: &mut Env::Drawer,
    );

    fn hover_interactions<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &Env,
        cursor: Rc<DynCursor<'a, Env>>,
        f: &mut dyn FnMut(&dyn Interaction),
    ) -> EventStatus;

    fn mouse_scroll<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &mut Env,
        cursor: Rc<DynCursor<'a, Env>>,
        amount: ScrollAmount,
    ) -> EventStatus;

    fn mouse_down<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &mut Env,
        cursor: Rc<DynCursor<'a, Env>>,
        button: MouseButton,
    ) -> EventStatus;

    fn focus<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &mut Env,
        keyboard: Rc<DynKeyboard<'a, Env>>,
        backward: bool,
    ) -> EventStatus;
}

impl<'a, Env: WidgetEnvironment + ?Sized> WidgetBase for DynWidget<'a, Env> {
    type Layout = DynWidgetLayout<'a, Env>;

    fn size(&self, layout: &Self::Layout) -> Size2i {
        self.0.size(layout)
    }
}

impl<'a, Env: WidgetEnvironment + ?Sized + 'a> Widget<Env> for DynWidget<'a, Env> {
    fn sizing(&self, env: &Env) -> Sizing {
        self.0.sizing(env)
    }

    fn layout(&self, env: &Env, size: Size2i) -> Box<DynWidgetLayout<'a, Env>> {
        unsafe { self.0.layout(env, size) }
    }

    fn relayout(&self, layout: &mut Self::Layout, env: &Env, size: Size2i) {
        self.0.relayout(layout, env, size)
    }

    fn outline<'b>(
        inst: WidgetInst<'b, 'b, Self>,
        outliner: &mut (impl WidgetOutliner<'b, Env> + ?Sized),
    ) {
        let widget = &inst.widget.0;
        widget.outline(inst.min, inst.layout, outliner.as_dyn_mut())
    }

    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer) {
        let widget = &inst.widget.0;
        widget.draw(inst.min, inst.layout, env, drawer)
    }

    fn hover_interactions<'b>(
        inst: WidgetInst<'b, '_, Self>,
        env: &Env,
        cursor: impl Cursor<'b, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        let widget = &inst.widget.0;
        widget.hover_interactions(inst.min, inst.layout, env, cursor.into_rc_dyn(), f)
    }

    fn mouse_scroll<'b>(
        inst: WidgetInst<'b, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'b, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus {
        let widget = &inst.widget.0;
        widget.mouse_scroll(inst.min, inst.layout, env, cursor.into_rc_dyn(), amount)
    }

    fn mouse_down<'b>(
        inst: WidgetInst<'b, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'b, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus {
        let widget = &inst.widget.0;
        widget.mouse_down(inst.min, inst.layout, env, cursor.into_rc_dyn(), button)
    }

    fn focus<'b>(
        inst: WidgetInst<'b, '_, Self>,
        env: &mut Env,
        keyboard: impl Keyboard<'b, Env> + Clone,
        backward: bool,
    ) -> EventStatus {
        let widget = &inst.widget.0;
        widget.focus(inst.min, inst.layout, env, keyboard.into_rc_dyn(), backward)
    }
}

impl<Env: WidgetEnvironment + ?Sized, W: Widget<Env>> WidgetSafe<Env> for W {
    fn size(&self, layout: &DynWidgetLayout<Env>) -> Size2i {
        <W as WidgetBase>::size(self, layout.downcast_ref::<W>())
    }

    fn sizing(&self, env: &Env) -> Sizing {
        <W as Widget<Env>>::sizing(self, env)
    }

    unsafe fn layout(&self, env: &Env, size: Size2i) -> Box<DynWidgetLayout<'static, Env>> {
        let source = Box::new(<W as Widget<Env>>::layout(self, env, size));
        let source: Box<DynWidgetLayout<'_, Env>> = DynWidgetLayout::from_box(source);
        std::mem::transmute(source)
    }

    fn relayout<'a>(&self, layout: &mut DynWidgetLayout<'a, Env>, env: &Env, size: Size2i)
    where
        W: 'a,
    {
        <W as Widget<Env>>::relayout(self, layout.downcast_mut::<W>(), env, size)
    }

    fn outline<'a>(
        &'a self,
        min: Vector2i,
        layout: &'a DynWidgetLayout<Env>,
        outliner: &mut dyn WidgetOutliner<'a, Env>,
    ) {
        WidgetInst {
            widget: self,
            min,
            layout: layout.downcast_ref::<W>(),
        }
        .outline(outliner)
    }

    fn draw(
        &self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &Env,
        drawer: &mut <Env as WidgetEnvironment>::Drawer,
    ) {
        WidgetInst {
            widget: self,
            min,
            layout: layout.downcast_ref::<W>(),
        }
        .draw(env, drawer)
    }

    fn hover_interactions<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &Env,
        cursor: Rc<DynCursor<'a, Env>>,
        f: &mut dyn FnMut(&dyn Interaction),
    ) -> EventStatus {
        WidgetInst {
            widget: self,
            min,
            layout: layout.downcast_ref::<W>(),
        }
        .hover_interactions(env, cursor, &mut |i| f(i))
    }

    fn mouse_scroll<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &mut Env,
        cursor: Rc<DynCursor<'a, Env>>,
        amount: ScrollAmount,
    ) -> EventStatus {
        WidgetInst {
            widget: self,
            min,
            layout: layout.downcast_ref::<W>(),
        }
        .mouse_scroll(env, cursor, amount)
    }

    fn mouse_down<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &mut Env,
        cursor: Rc<DynCursor<'a, Env>>,
        button: MouseButton,
    ) -> EventStatus {
        WidgetInst {
            widget: self,
            min,
            layout: layout.downcast_ref::<W>(),
        }
        .mouse_down(env, cursor, button)
    }

    fn focus<'a>(
        &'a self,
        min: Vector2i,
        layout: &DynWidgetLayout<Env>,
        env: &mut Env,
        keyboard: Rc<DynKeyboard<'a, Env>>,
        backward: bool,
    ) -> EventStatus {
        WidgetInst {
            widget: self,
            min,
            layout: layout.downcast_ref::<W>(),
        }
        .focus(env, keyboard, backward)
    }
}

/// The layout for a [`DynWidget`].
#[repr(transparent)]
pub struct DynWidgetLayout<'a, Env: WidgetEnvironment + ?Sized>(dyn WidgetLayoutSafe<Env> + 'a);

impl<'a, Env: WidgetEnvironment + ?Sized> DynWidgetLayout<'a, Env> {
    /// Converts a reference to a layout into a reference to a [`DynWidgetLayout`].
    pub fn from_ref<T: 'a>(source: &T) -> &Self {
        let source: &(dyn WidgetLayoutSafe<Env> + 'a) = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts a boxed layout into a boxed [`DynWidgetLayout`].
    pub fn from_box<T: 'a>(source: Box<T>) -> Box<Self> {
        let source: Box<dyn WidgetLayoutSafe<Env> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Downcasts this layout for use with a specific type of [`Widget`]. Will panic if the layout
    /// is not compatible with the widget.
    pub fn downcast_ref<W: Widget<Env> + 'a>(&self) -> &W::Layout {
        // TODO: Check type
        let owned = unsafe {
            &*(&self.0 as *const dyn WidgetLayoutSafe<Env>
                as *const <W::Layout as WidgetLayout>::Owned)
        };
        owned.borrow()
    }

    /// Downcasts this layout for use with a specific type of [`Widget`]. Will panic if the layout
    /// is not compatible with the widget.
    pub fn downcast_mut<W: Widget<Env> + 'a>(&mut self) -> &mut W::Layout {
        // TODO: Check type
        let owned = unsafe {
            &mut *(&mut self.0 as *mut dyn WidgetLayoutSafe<Env>
                as *mut <W::Layout as WidgetLayout>::Owned)
        };
        owned.borrow_mut()
    }
}

impl<'a, Env: WidgetEnvironment + ?Sized> WidgetLayout for DynWidgetLayout<'a, Env> {
    type Owned = Box<Self>;
}

/// An object-safe variant of [`WidgetLayout`].
trait WidgetLayoutSafe<Env: WidgetEnvironment + ?Sized> {}

impl<Env: WidgetEnvironment + ?Sized, T> WidgetLayoutSafe<Env> for T {}
