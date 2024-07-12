use super::*;

/// A [`Cursor`] of an unspecified type, determined at runtime.
#[repr(transparent)]
pub struct DynCursor<'cursor, Env: WidgetEnvironment + ?Sized + 'cursor>(
    dyn CursorSafe<'cursor, Env> + 'cursor,
);

impl<'cursor, Env: WidgetEnvironment + ?Sized> DynCursor<'cursor, Env> {
    /// Converts a [`Rc`]-wrapped [`Cursor`] into a wrapped [`DynCursor`].
    pub fn from_rc<T: Cursor<'cursor, Env> + 'cursor>(source: Rc<T>) -> Rc<Self> {
        let source: Rc<dyn CursorSafe<'cursor, Env> + 'cursor> = source;
        unsafe { std::mem::transmute(source) }
    }
}

/// An object-safe variant of [`Cursor`].
trait CursorSafe<'cursor, Env: WidgetEnvironment + ?Sized>: 'cursor {
    fn pos(&self, env: &Env) -> Point2i;
    fn keyboard(&self, env: &Env) -> Option<Rc<DynKeyboard<'cursor, Env>>>;
    fn set_handler(&self, env: &mut Env, handler: Box<DynCursorHandler<'cursor, Env>>);
    fn clear_handler(&self, env: &mut Env);
    fn default_interactions(&self, env: &Env, f: &mut dyn FnMut(&dyn Interaction)) -> EventStatus;
    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus;
    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus;
}

impl<'cursor, Env: WidgetEnvironment + ?Sized> Cursor<'cursor, Env> for DynCursor<'cursor, Env> {
    fn pos(&self, env: &Env) -> Point2i {
        self.0.pos(env)
    }

    type Keyboard = Rc<DynKeyboard<'cursor, Env>>;
    fn keyboard(&self, env: &Env) -> Option<Self::Keyboard> {
        self.0.keyboard(env)
    }

    fn set_handler(&self, env: &mut Env, handler: impl CursorHandler<Env> + 'cursor) {
        let handler = DynCursorHandler::from_box(Box::new(handler));
        self.0.set_handler(env, handler)
    }

    fn clear_handler(&self, env: &mut Env) {
        self.0.clear_handler(env)
    }

    fn default_interactions(&self, env: &Env, mut f: impl FnMut(&dyn Interaction)) -> EventStatus {
        self.0.default_interactions(env, &mut f)
    }

    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus {
        self.0.default_mouse_scroll(env, amount)
    }

    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus {
        self.0.default_mouse_down(env, button)
    }
}

impl<'cursor, Env: WidgetEnvironment + ?Sized> Cursor<'cursor, Env>
    for Rc<DynCursor<'cursor, Env>>
{
    fn pos(&self, env: &Env) -> Point2i {
        Cursor::pos(&**self, env)
    }

    type Keyboard = Rc<DynKeyboard<'cursor, Env>>;
    fn keyboard(&self, env: &Env) -> Option<Self::Keyboard> {
        Cursor::keyboard(&**self, env)
    }

    fn set_handler(&self, env: &mut Env, handler: impl CursorHandler<Env> + 'cursor) {
        Cursor::set_handler(&**self, env, handler)
    }

    fn clear_handler(&self, env: &mut Env) {
        Cursor::clear_handler(&**self, env)
    }

    fn default_interactions(&self, env: &Env, f: impl FnMut(&dyn Interaction)) -> EventStatus {
        Cursor::default_interactions(&**self, env, f)
    }

    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus {
        Cursor::default_mouse_scroll(&**self, env, amount)
    }

    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus {
        Cursor::default_mouse_down(&**self, env, button)
    }

    fn into_rc_dyn(self) -> Rc<DynCursor<'cursor, Env>> {
        self
    }
}

impl<'cursor, Env: WidgetEnvironment + ?Sized + 'cursor, T: Cursor<'cursor, Env>>
    CursorSafe<'cursor, Env> for T
{
    fn pos(&self, env: &Env) -> Point2i {
        <T as Cursor<'cursor, Env>>::pos(self, env)
    }

    fn keyboard(&self, env: &Env) -> Option<Rc<DynKeyboard<'cursor, Env>>> {
        <T as Cursor<'cursor, Env>>::keyboard(self, env).map(|keyboard| keyboard.into_rc_dyn())
    }

    fn set_handler(&self, env: &mut Env, handler: Box<DynCursorHandler<'cursor, Env>>) {
        <T as Cursor<'cursor, Env>>::set_handler(self, env, handler)
    }

    fn clear_handler(&self, env: &mut Env) {
        <T as Cursor<'cursor, Env>>::clear_handler(self, env)
    }

    fn default_interactions(&self, env: &Env, f: &mut dyn FnMut(&dyn Interaction)) -> EventStatus {
        <T as Cursor<'cursor, Env>>::default_interactions(self, env, f)
    }

    fn default_mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) -> EventStatus {
        <T as Cursor<'cursor, Env>>::default_mouse_scroll(self, env, amount)
    }

    fn default_mouse_down(&self, env: &mut Env, button: MouseButton) -> EventStatus {
        <T as Cursor<'cursor, Env>>::default_mouse_down(self, env, button)
    }
}

/// A [`CursorHandler`] of an unspecified type, determined at runtime.
#[repr(transparent)]
pub struct DynCursorHandler<'a, Env: WidgetEnvironment + ?Sized>(dyn CursorHandlerSafe<Env> + 'a);

impl<'a, Env: WidgetEnvironment + ?Sized> DynCursorHandler<'a, Env> {
    /// Converts a boxed [`CursorHandler`] into a boxed [`DynCursorHandler`].
    pub fn from_box<T: CursorHandler<Env> + 'a>(source: Box<T>) -> Box<Self> {
        let source: Box<dyn CursorHandlerSafe<Env> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }

    /// Converts an [`Rc`]-wrapped [`CursorHandler`] into a wrapped [`DynCursorHandler`].
    pub fn from_rc<T: CursorHandler<Env> + 'a>(source: Rc<T>) -> Rc<Self> {
        let source: Rc<dyn CursorHandlerSafe<Env> + 'a> = source;
        unsafe { std::mem::transmute(source) }
    }
}

/// An object-safe variant of [`CursorHandler`].
trait CursorHandlerSafe<Env: WidgetEnvironment + ?Sized> {
    fn interactions(&self, env: &Env, f: &mut dyn FnMut(&dyn Interaction));
    fn mouse_scroll(&self, env: &mut Env, amount: ScrollAmount);
    fn mouse_down(&self, env: &mut Env, button: MouseButton);
    fn mouse_up(&self, env: &mut Env, button: MouseButton);
}

impl<Env: WidgetEnvironment + ?Sized> CursorHandler<Env> for DynCursorHandler<'_, Env> {
    fn interactions(&self, env: &Env, mut f: impl FnMut(&dyn Interaction)) {
        self.0.interactions(env, &mut f)
    }

    fn mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) {
        self.0.mouse_scroll(env, amount)
    }

    fn mouse_down(&self, env: &mut Env, button: MouseButton) {
        self.0.mouse_down(env, button)
    }

    fn mouse_up(&self, env: &mut Env, button: MouseButton) {
        self.0.mouse_up(env, button)
    }
}

impl<Env: WidgetEnvironment + ?Sized, T: CursorHandler<Env>> CursorHandlerSafe<Env> for T {
    fn interactions(&self, env: &Env, f: &mut dyn FnMut(&dyn Interaction)) {
        <T as CursorHandler<Env>>::interactions(self, env, f)
    }

    fn mouse_scroll(&self, env: &mut Env, amount: ScrollAmount) {
        <T as CursorHandler<Env>>::mouse_scroll(self, env, amount)
    }

    fn mouse_down(&self, env: &mut Env, button: MouseButton) {
        <T as CursorHandler<Env>>::mouse_down(self, env, button)
    }

    fn mouse_up(&self, env: &mut Env, button: MouseButton) {
        <T as CursorHandler<Env>>::mouse_up(self, env, button)
    }
}

/// A [`Keyboard`] of an unspecified type, determined at runtime.
#[repr(transparent)]
pub struct DynKeyboard<'keyboard, Env: WidgetEnvironment + ?Sized + 'keyboard>(
    dyn KeyboardSafe<'keyboard, Env> + 'keyboard,
);

impl<'keyboard, Env: WidgetEnvironment + ?Sized> DynKeyboard<'keyboard, Env> {
    /// Converts a [`Rc`]-wrapped [`Keyboard`] into a wrapped [`DynKeyboard`].
    pub fn from_rc<T: Keyboard<'keyboard, Env> + 'keyboard>(source: Rc<T>) -> Rc<Self> {
        let source: Rc<dyn KeyboardSafe<'keyboard, Env> + 'keyboard> = source;
        unsafe { std::mem::transmute(source) }
    }
}

/// An object-safe variant of [`Keyboard`].
trait KeyboardSafe<'keyboard, Env: WidgetEnvironment + ?Sized>: 'keyboard {
    fn keys_held(&self, env: &Env, held: &mut dyn FnMut(Key));
    fn set_handler(&self, env: &mut Env, handler: Box<DynKeyboardHandler<'keyboard, Env>>);
    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus;
}

impl<'keyboard, Env: WidgetEnvironment + ?Sized + 'keyboard> Keyboard<'keyboard, Env>
    for DynKeyboard<'keyboard, Env>
{
    fn keys_held(&self, env: &Env, mut held: impl FnMut(Key)) {
        self.0.keys_held(env, &mut held)
    }

    fn set_handler(&self, env: &mut Env, handler: impl KeyboardHandler<Env> + 'keyboard) {
        self.0.set_handler(env, Box::new(handler))
    }

    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus {
        self.0.default_key_down(env, key)
    }
}

impl<'keyboard, Env: WidgetEnvironment + ?Sized> Keyboard<'keyboard, Env>
    for Rc<DynKeyboard<'keyboard, Env>>
{
    fn keys_held(&self, env: &Env, held: impl FnMut(Key)) {
        Keyboard::keys_held(&**self, env, held)
    }

    fn set_handler(&self, env: &mut Env, handler: impl KeyboardHandler<Env> + 'keyboard) {
        Keyboard::set_handler(&**self, env, handler)
    }

    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus {
        Keyboard::default_key_down(&**self, env, key)
    }

    fn into_rc_dyn(self) -> Rc<DynKeyboard<'keyboard, Env>> {
        self
    }
}

impl<'keyboard, Env: WidgetEnvironment + ?Sized + 'keyboard, T: Keyboard<'keyboard, Env>>
    KeyboardSafe<'keyboard, Env> for T
{
    fn keys_held(&self, env: &Env, held: &mut dyn FnMut(Key)) {
        <T as Keyboard<'keyboard, Env>>::keys_held(self, env, held)
    }

    fn set_handler(&self, env: &mut Env, handler: Box<DynKeyboardHandler<'keyboard, Env>>) {
        <T as Keyboard<'keyboard, Env>>::set_handler(self, env, handler)
    }

    fn default_key_down(&self, env: &mut Env, key: Key) -> EventStatus {
        <T as Keyboard<'keyboard, Env>>::default_key_down(self, env, key)
    }
}

/// A [`KeyboardHandler`] of an unspecified type, determined at runtime.
pub type DynKeyboardHandler<'a, Env> = dyn KeyboardHandler<Env> + 'a;
