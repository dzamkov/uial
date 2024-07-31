use super::*;

/// A wrapper over a [`WgpuDrawer`] which hides the `'pass` lifetime.
#[repr(transparent)]
pub struct WgpuErasedDrawer {
    // We lie about the lifetime here. It is actually somewhere between the lifetime of the
    // drawer reference, and `'static`.
    _source: WgpuDrawer<'static>
}

impl WgpuErasedDrawer {
    /// Type-erases a reference to a [`WgpuDrawer`].
    pub fn from_ref<'a>(source: &'a WgpuDrawer<'_>) -> &'a WgpuErasedDrawer {
        unsafe { std::mem::transmute(source) }
    }

    /// Type-erases a mutable reference to a [`WgpuDrawer`].
    pub fn from_mut<'a>(source: &'a mut WgpuDrawer<'_>) -> &'a mut WgpuErasedDrawer {
        unsafe { std::mem::transmute(source) }
    }

    /// Calls a function with the unerased form of this [`WgpuErasedDrawer`].
    pub fn with_unerase_ref<'a, R>(&'a self, f: impl FnOnce(&'a WgpuDrawer) -> R) -> R {
        f(unsafe { std::mem::transmute(self) })
    }

    /// Calls a function with the unerased form of this [`WgpuErasedDrawer`].
    pub fn with_unerase_mut<'a, R>(&'a mut self, f: impl FnOnce(&'a mut WgpuDrawer) -> R) -> R {
        f(unsafe { std::mem::transmute(self) })
    }
}

impl VectorDrawer for WgpuErasedDrawer {
    fn visible_bounds(&self) -> Box2 {
        self.with_unerase_ref(|d| d.visible_bounds())
    }

    fn draw_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcDrawer<'_> {
        self.with_unerase_mut(|d| d.draw_polyarc(paint, start))
    }

    type PolyarcDrawer<'a> = ArcTessellator<WgpuPolylineDrawer<'a>>
    where
        Self: 'a;

    fn fill_polyarc(&mut self, paint: Paint, start: Vector2) -> Self::PolyarcFiller<'_> {
        self.with_unerase_mut(|d| d.fill_polyarc(paint, start))
    }

    type PolyarcFiller<'a> = ArcTessellator<WgpuPolylineFiller<'a>>
    where
        Self: 'a;
}

impl RasterDrawer for WgpuErasedDrawer {
    fn fill_rect(&mut self, paint: Paint, rect: Box2i) {
        self.with_unerase_mut(|d| d.fill_rect(paint, rect))
    }
}

impl ImageDrawer<WgpuImageAtlas<'_>> for WgpuErasedDrawer {
    fn draw_image(
        &mut self,
        image: ImageSource<WgpuImageAtlas<'_>>,
        paint: Paint,
        trans: Similarity2i,
    ) {
        self.with_unerase_mut(|d| d.draw_image(image, paint, trans))
    }
}