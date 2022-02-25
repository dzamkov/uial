use crate::*;

/// A set of [`Vector2`]s each associated with a relative "cost". This can be used to describe the
/// allowed sizes of a graphical element and establish a preference hierarchy between them.
#[derive(fortify::Lower, Clone)]
pub struct Sizing<S> {
    /// An ordered list of entries which define this [`Sizing`]. All valid sizes are partitioned
    /// into height "bands", which are further divided into rectangular "regions". Each region is
    /// described by a [`SizingEntry`], which specifies the minimum coordinate and cost of that
    /// region. Entries are ordered first by X, then by Y (i.e. band), allowing us to use binary
    /// search to determine which region a particular size is in.
    entries: Box<[SizingEntry<S>]>,
}

#[derive(Clone, Copy)]
struct SizingEntry<S> {
    min: Vector2<S>,

    /// The cost associated with this entry, or `u32::MAX` if sizes in the defined region are not
    /// valid. The minimum entry cost of a [`Sizing`] must be normalized to 0.
    cost: u32,
}

impl<S> SizingEntry<S> {
    pub fn new(min: Vector2<S>, cost: u32) -> Self {
        Self { min, cost }
    }
}

impl<S: BaseNum> Sizing<S> {
    /// Constructs a [`Sizing`] which permits any size with equal preference.
    pub fn any() -> Self {
        Self {
            entries: Box::new([
                SizingEntry::new(vec2(S::zero(), S::zero()), 0),
            ])
        }
    }

    /// Constructs a [`Sizing`] which only permits sizes within the given range.
    pub fn in_range(range: Box2<S>) -> Self {
        Self {
            entries: match (range.min.x <= S::zero(), range.min.y <= S::zero()) {
                (true, true) => todo!(),
                (true, false) => todo!(),
                (false, true) => todo!(),
                (false, false) => Box::new([
                    SizingEntry::new(vec2(S::zero(), S::zero()), u32::MAX),
                    SizingEntry::new(vec2(S::zero(), range.min.y), u32::MAX),
                    SizingEntry::new(vec2(range.min.x, range.min.y), 0),
                    SizingEntry::new(vec2(range.max.x, range.min.y), u32::MAX),
                    SizingEntry::new(vec2(S::zero(), range.max.y), u32::MAX),
                ]),
            },
        }
    }
}

impl Sizing<i32> {
    /// Constructs a [`Sizing`] which permits only the given size.
    pub fn exact(size: Vector2<i32>) -> Self {
        Self::in_range(box2(size.x, size.x + 1, size.y, size.y + 1))
    }
}