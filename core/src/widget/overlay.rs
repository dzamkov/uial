use super::*;

/// A [`Widget`] which combines two source [`Widget`]s in the same layout rectangle, overlaying
/// one on top of the other.
pub struct Overlay<Below, Above> {
    below: Below,
    above: Above,
}

/// Constructs a [`Widget`] which overlays one widget above another in the same layout rectangle.
pub fn overlay<Below: WidgetBase, Above: WidgetBase>(
    below: Below,
    above: Above,
) -> Overlay<Below, Above> {
    Overlay { below, above }
}

impl<Below: WidgetBase, Above: WidgetBase> WidgetBase for Overlay<Below, Above> {
    type Layout =
        Overlay<<Below::Layout as WidgetLayout>::Owned, <Above::Layout as WidgetLayout>::Owned>;
    fn size(&self, layout: &Self::Layout) -> Size2i {
        let size = Below::size(&self.below, layout.below.borrow());
        debug_assert_eq!(size, Above::size(&self.above, layout.above.borrow()));
        size
    }
}

impl<Env: WidgetEnvironment + ?Sized, Below: Widget<Env>, Above: Widget<Env>> Widget<Env>
    for Overlay<Below, Above>
{
    fn sizing(&self, env: &Env) -> Sizing {
        &self.below.sizing(env) & &self.above.sizing(env)
    }

    fn layout(&self, env: &Env, size: Size2i) -> Self::Layout {
        Overlay {
            below: self.below.layout(env, size),
            above: self.above.layout(env, size),
        }
    }

    fn relayout(&self, layout: &mut Self::Layout, env: &Env, size: Size2i) {
        self.below.relayout(layout.below.borrow_mut(), env, size);
        self.above.relayout(layout.above.borrow_mut(), env, size);
    }

    fn outline<'a>(
        inst: WidgetInst<'a, 'a, Self>,
        outliner: &mut (impl WidgetOutliner<'a, Env> + ?Sized),
    ) {
        inst.below().outline(outliner);
        inst.above().outline(outliner);
    }

    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer) {
        inst.below().draw(env, drawer);
        inst.above().draw(env, drawer);
    }

    fn hover_interactions<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &Env,
        cursor: impl Cursor<'a, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        if let EventStatus::Handled = inst.above().hover_interactions(env, cursor.clone(), f) {
            return EventStatus::Handled;
        }
        inst.below().hover_interactions(env, cursor, f)
    }

    fn mouse_scroll<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus {
        if let EventStatus::Handled = inst.above().mouse_scroll(env, cursor.clone(), amount) {
            return EventStatus::Handled;
        }
        inst.below().mouse_scroll(env, cursor, amount)
    }

    fn mouse_down<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus {
        if let EventStatus::Handled = inst.above().mouse_down(env, cursor.clone(), button) {
            return EventStatus::Handled;
        }
        inst.below().mouse_down(env, cursor, button)
    }

    fn focus<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        keyboard: impl Keyboard<'a, Env> + Clone,
        backward: bool,
    ) -> EventStatus {
        if let EventStatus::Handled = inst.above().focus(env, keyboard.clone(), backward) {
            return EventStatus::Handled;
        }
        inst.below().focus(env, keyboard, backward)
    }
}

/// Contains extension methods for [`WidgetInst`]s of an [`Overlay`] widget.
pub trait OverlayInstExt<'a, 'b, Below: WidgetBase, Above: WidgetBase> {
    /// Gets the [`WidgetInst`] for the lower widget in the overlay.
    fn below(&self) -> WidgetInst<'a, 'b, Below>;

    /// Gets the [`WidgetInst`] for the higher widget in the overlay.
    fn above(&self) -> WidgetInst<'a, 'b, Above>;
}

impl<'a, 'b, Below: WidgetBase, Above: WidgetBase> OverlayInstExt<'a, 'b, Below, Above>
    for WidgetInst<'a, 'b, Overlay<Below, Above>>
{
    fn below(&self) -> WidgetInst<'a, 'b, Below> {
        WidgetInst {
            widget: &self.widget.below,
            min: self.min,
            layout: self.layout.below.borrow(),
        }
    }

    fn above(&self) -> WidgetInst<'a, 'b, Above> {
        WidgetInst {
            widget: &self.widget.above,
            min: self.min,
            layout: self.layout.above.borrow(),
        }
    }
}
