mod canvas;
mod dynamic;
mod extend;
mod fill;
mod empty;
mod on_key;
mod overlay;
mod pad;
mod restrict;
mod switch;
mod zoom_canvas;
mod stack;

use super::*;
pub use canvas::*;
pub use dynamic::*;
pub use extend::*;
pub use fill::*;
pub use empty::*;
pub use on_key::*;
pub use overlay::*;
pub use pad::*;
pub use restrict::*;
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use std::rc::Rc;
pub use switch::*;
pub use zoom_canvas::*;
pub use stack::*;

pub trait WidgetBase {
    /// Describes a possible layout for this [`Widget`]. This encapsulates information such as
    /// the [`Widget`]s size and the relative placement of its internal contents.
    type Layout: WidgetLayout + ?Sized;

    /// Gets the size of this [`Widget`] with the given layout.
    fn size(&self, layout: &Self::Layout) -> Size2i;
}

/// Describes a possible layout for some type of [`Widget`].
pub trait WidgetLayout {
    /// The owned (sized) form of this layout. If the layout is sized, this will just be the
    /// layout itself.
    type Owned: Sized + BorrowMut<Self>;
}

impl<T> WidgetLayout for T {
    type Owned = T;
}

/// Describes an interactive UI component that is displayed within a rectangle in discrete
/// two-dimensional space. `Env` encapsulates the external data and/or resources that the
/// widget has access to.
pub trait Widget<Env: WidgetEnvironment + ?Sized>: WidgetBase {
    /// Gets the sizing preferences/constraints for this [`Widget`].
    fn sizing(&self, env: &Env) -> Sizing;

    /// Constructs a layout for this [`Widget`] with the given size. The size should satisfy the
    /// constraints returned by [`Widget::sizing`].
    fn layout(&self, env: &Env, size: Size2i) -> <Self::Layout as WidgetLayout>::Owned;

    /// Updates a layout in response to a change in size or environment. This is usually faster
    /// than creating a new layout from scratch.
    fn relayout(&self, layout: &mut Self::Layout, env: &Env, size: Size2i);

    /// Builds an outline of the contents of this [`Widget`].
    fn outline<'a>(
        inst: WidgetInst<'a, 'a, Self>,
        outliner: &mut (impl WidgetOutliner<'a, Env> + ?Sized),
    ) {
        // Nothing by default
        let _ = (inst, outliner);
    }

    /// Draws an instance of this [`Widget`] to the given drawer.
    fn draw(inst: WidgetInst<Self>, env: &Env, drawer: &mut Env::Drawer);

    /// Calls the given function for each [`Interaction`] arising from the given [`Cursor`]
    /// hovering over this widget. The returned [`EventStatus`] is used to signal to higher-level
    /// widgets whether whether mouse events will be handled by this widget (i.e. to suppress
    /// interactions from other widgets for the same cursor).
    fn hover_interactions<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &Env,
        cursor: impl Cursor<'a, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus {
        // None by default
        let _ = (inst, env, cursor, f);
        EventStatus::Bubble
    }

    /// Responds to a mouse scroll for a cursor within the bounds of this widget. This will only
    /// be called when the [`CursorHandler`] for the cursor defers to the default event handler.
    /// The returned [`EventStatus`] is used to signal to higher-level widgets whether the
    /// event has been handled.
    fn mouse_scroll<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus {
        // Not handled by default
        let _ = (inst, env, cursor, amount);
        EventStatus::Bubble
    }

    /// Responds to a mouse button being pressed for a cursor within the bounds of this widget.
    /// The [`CursorHandler`] for the cursor has priority for this event. This will only
    /// be called when the [`CursorHandler`] for the cursor defers to the default event handler.
    /// The returned [`EventStatus`] is used to signal to higher-level widgets whether the
    /// event has been handled.
    fn mouse_down<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus {
        // Not handled by default
        let _ = (inst, env, cursor, button);
        EventStatus::Bubble
    }

    /// Responds to keyboard focus entering the widget.
    fn focus<'a>(
        inst: WidgetInst<'a, '_, Self>,
        env: &mut Env,
        keyboard: impl Keyboard<'a, Env> + Clone,
        backward: bool,
    ) -> EventStatus {
        // Not handled by default
        let _ = (inst, env, keyboard, backward);
        EventStatus::Bubble
    }

    /// Converts this [`Widget`] into a boxed [`DynWidget`].
    fn into_boxed_dyn<'a>(self) -> Box<DynWidget<'a, Env>>
    where
        Self: Sized + 'a,
    {
        DynWidget::from_box(Box::new(self))
    }

    /// Converts this [`Widget`] into a [`Rc`]-wrapped [`DynWidget`].
    fn into_rc_dyn<'a>(self) -> Rc<DynWidget<'a, Env>>
    where
        Self: Sized + 'a,
    {
        DynWidget::from_rc(Rc::new(self))
    }
}

/// A [`Widget`] which wraps a single child [`Widget`].
pub trait WidgetInner: WidgetBase {
    /// The type of the inner widget.
    type Inner: WidgetBase;

    /// Gets the [`WidgetInst`] for the child widget of the given [`WidgetInst`].
    fn inner<'a, 'b>(inst: WidgetInst<'a, 'b, Self>) -> WidgetInst<'a, 'b, Self::Inner>;
}

/// An interface for creating an "outline" of a [`Widget`] tree.
pub trait WidgetOutliner<'a, Env: WidgetEnvironment + ?Sized> {
    /// Adds a widget to the outline.
    fn place_dyn(&mut self, inst: WidgetInst<'a, 'a, DynWidget<'a, Env>>);

    /// Upcasts a mutable reference to this [`WidgetOutliner`].
    fn as_dyn_mut(&mut self) -> &mut dyn WidgetOutliner<'a, Env>;
}

/// Contains extension methods for [`WidgetOutliner`].
pub trait WidgetOutlinerExt<'a, Env: WidgetEnvironment + ?Sized + 'a>:
    WidgetOutliner<'a, Env>
{
    /// Adds a widget to the outline.
    fn place<W: Widget<Env>>(&mut self, inst: WidgetInst<'a, 'a, W>)
    where
        W::Layout: Sized,
    {
        self.place_dyn(WidgetInst {
            widget: DynWidget::from_ref(inst.widget),
            min: inst.min,
            layout: DynWidgetLayout::from_ref(inst.layout),
        })
    }
}

impl<'a, Env: WidgetEnvironment + ?Sized + 'a, T: WidgetOutliner<'a, Env> + ?Sized>
    WidgetOutlinerExt<'a, Env> for T
{
}

/// Combines information about a [`Widget`], its layout, and its placement in some discrete space.
pub struct WidgetInst<'a: 'b, 'b, W: WidgetBase + ?Sized> {
    pub widget: &'a W,
    pub min: Vector2i,
    pub layout: &'b W::Layout,
}

impl<'a: 'b, 'b, W: WidgetBase + ?Sized> WidgetInst<'a, 'b, W> {
    /// Gets the bounding rectangle for the instance.
    pub fn bounds(&self) -> Box2i {
        Box2i::from_min_size(self.min, self.widget.size(self.layout))
    }

    /// Builds an outline of the contents of this [`Widget`].
    pub fn outline<Env: WidgetEnvironment + ?Sized>(
        &self,
        outliner: &mut (impl WidgetOutliner<'b, Env> + ?Sized),
    ) where
        W: Widget<Env>,
    {
        W::outline(*self, outliner)
    }

    /// Calls the given function for each [`Interaction`] arising from the given [`Cursor`]
    /// hovering over this widget. The returned [`EventStatus`] is used to signal to higher-level
    /// widgets whether whether mouse events will be handled by this widget (i.e. to suppress
    /// interactions from other widgets for the same cursor).
    pub fn hover_interactions<Env: WidgetEnvironment + ?Sized>(
        &self,
        env: &Env,
        cursor: impl Cursor<'a, Env> + Clone,
        f: &mut impl FnMut(&dyn Interaction),
    ) -> EventStatus
    where
        W: Widget<Env>,
    {
        W::hover_interactions(*self, env, cursor, f)
    }

    /// Responds to a mouse scroll for a cursor within the bounds of this widget. This will only
    /// be called when the [`CursorHandler`] for the cursor defers to the default event handler.
    /// The returned [`EventStatus`] is used to signal to higher-level widgets whether the
    /// event has been handled.
    pub fn mouse_scroll<Env: WidgetEnvironment + ?Sized>(
        &self,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        amount: ScrollAmount,
    ) -> EventStatus
    where
        W: Widget<Env>,
    {
        W::mouse_scroll(*self, env, cursor, amount)
    }

    /// Responds to a mouse button being pressed for a cursor within the bounds of this widget.
    /// The [`CursorHandler`] for the cursor has priority for this event. This will only
    /// be called when the [`CursorHandler`] for the cursor defers to the default event handler.
    /// The returned [`EventStatus`] is used to signal to higher-level widgets whether the
    /// event has been handled.
    pub fn mouse_down<Env: WidgetEnvironment + ?Sized>(
        &self,
        env: &mut Env,
        cursor: impl Cursor<'a, Env> + Clone,
        button: MouseButton,
    ) -> EventStatus
    where
        W: Widget<Env>,
    {
        W::mouse_down(*self, env, cursor, button)
    }

    /// Responds to keyboard focus entering the widget.
    pub fn focus<Env: WidgetEnvironment + ?Sized>(
        &self,
        env: &mut Env,
        keyboard: impl Keyboard<'a, Env> + Clone,
        backward: bool,
    ) -> EventStatus
    where
        W: Widget<Env>,
    {
        W::focus(*self, env, keyboard, backward)
    }

    /// Draws this instance to the given drawer.
    pub fn draw<Env: WidgetEnvironment + ?Sized>(&self, env: &Env, drawer: &mut Env::Drawer)
    where
        W: Widget<Env>,
    {
        W::draw(*self, env, drawer)
    }

    /// Gets the instance for the child widget of this instance.
    pub fn inner(&self) -> WidgetInst<'a, 'b, W::Inner>
    where
        W: WidgetInner,
    {
        W::inner(*self)
    }
}

impl<'a: 'b, 'b, W: WidgetBase + ?Sized> Clone for WidgetInst<'a, 'b, W> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a: 'b, 'b, W: WidgetBase + ?Sized> Copy for WidgetInst<'a, 'b, W> {}

/// A valid environment for a [`Widget`]. This encapsulates the external data and/or resources that
/// the widget has access to.
pub trait WidgetEnvironment {
    /// The type of drawing interface provided to [`Widget`]s to draw themselves in this
    /// environment. The drawer may directly reference data stored in the [`WidgetInst`], hence
    /// the `'inst` lifetime.
    type Drawer: ?Sized;

    /// Calls the given function for each [`Interaction`] that is currently occuring in this
    /// environment.
    fn interactions(&self, f: impl FnMut(&dyn Interaction));

    /// Gets the current placement of the given [`Widget`] in this environment, [`None`] if the
    /// widget hasn't been instantiated and/or is not visible. This requires that
    /// the widget is included in the outline produced by [`WidgetBase::outline`] of the top-level
    /// widget for the environment.
    fn with_locate<'a, W: Widget<Self>, R>(
        &self,
        widget: &'a W,
        f: impl FnOnce(WidgetInst<'a, '_, W>) -> R,
    ) -> Option<R>;
}

/// A static "outline" of a [`Widget`] tree which allows efficient addressing of widgets.
pub struct WidgetOutline<'a, Env: WidgetEnvironment + ?Sized> {
    widgets: HashMap<*const DynWidget<'a, Env>, (Vector2i, &'a DynWidgetLayout<'a, Env>)>,
}

impl<'a, Env: WidgetEnvironment + ?Sized> WidgetOutline<'a, Env> {
    /// Creates a new initially-empty [`WidgetOutline`].
    pub fn new() -> Self {
        Self {
            widgets: HashMap::new(),
        }
    }

    /// Attempts to get the instance for the given [`Widget`] in this outline, or returns
    /// [`None`] if the widget isn't included in the outline.
    pub fn locate_dyn(
        &self,
        widget: &'a DynWidget<'a, Env>,
    ) -> Option<WidgetInst<'a, 'a, DynWidget<'a, Env>>> {
        self.widgets
            .get(&(widget as *const DynWidget<'a, Env>))
            .map(|(min, layout)| WidgetInst {
                widget,
                min: *min,
                layout,
            })
    }

    /// Attempts to get the placement of the given [`Widget`] in this outline, or returns
    /// [`None`] if the widget isn't included in the outline.
    pub fn locate<'b: 'a, W: Widget<Env>>(&self, widget: &'b W) -> Option<WidgetInst<'b, 'a, W>> {
        self.widgets
            .get(&(DynWidget::from_ref(widget) as *const DynWidget<'a, Env>))
            .map(|(min, layout)| WidgetInst {
                widget,
                min: *min,
                layout: layout.downcast_ref::<W>(),
            })
    }
}

impl<Env: WidgetEnvironment + ?Sized> Default for WidgetOutline<'_, Env> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, Env: WidgetEnvironment + ?Sized> WidgetOutliner<'a, Env> for WidgetOutline<'a, Env> {
    fn place_dyn(&mut self, inst: WidgetInst<'a, 'a, DynWidget<'a, Env>>) {
        let occupied = self
            .widgets
            .insert(inst.widget, (inst.min, inst.layout))
            .is_some();
        assert!(!occupied);
    }

    fn as_dyn_mut(&mut self) -> &mut dyn WidgetOutliner<'a, Env> {
        self
    }
}
