use super::*;
use std::ops::{Add, AddAssign, BitAnd, BitOr};

/// An arbitrarily-complex set of [`Size2i`]s, typically used to represent the valid sizes of a
/// widget.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Sizing {
    /// The list of [`Sample`]s which defines this [`Sizing`].
    ///
    /// Samples in the list are lexicographically ordered by their `size`, first by X component,
    /// then by Y component.
    ///
    /// Two [`Sizing`]s represent the same set of [`Size2i`]s if and only if they have the same
    /// sample list.
    ///
    /// The *effective* sample for a given [`Size2i`] is defined as the last sample in this list
    /// for which the components of the sample's `size` are both less than or equal to the
    /// corresponding components of the given size. Whether a particular size is included in the
    /// set is determined by its effective sample. Sizes that do not have an effective sample are
    /// not in the set.
    samples: Box<[Sample]>,
}

/// A data point in a [`Sizing`] which determines the membership of [`Size2i`]s for which it is
/// effective (See [`Sizing::samples`]).
#[derive(Debug, PartialEq, Eq, Clone)]
struct Sample {
    size: Size2i,
    ty: SampleType,
}

/// Describes how a [`Sample`] affects the memberships of [`Size2i`]s for which it is effective.
#[derive(Debug, PartialEq, Eq, Clone)]
enum SampleType {
    /// [`Size2i`]s subject to the sample are not included in the [`Sizing`].
    Exclude,

    /// [`Size2i`]s subject to the sample are included in the [`Sizing`].
    Include,

    /// The membership of subject [`Size2i`]s is determined by subtracing a given "offset" from
    /// the [`Size2i`] and querying the [`Sizing`] again. The offset is equal to the difference
    /// of `size` for this sample and that of a prior sample in the `samples` list.
    ///
    /// This can be used to define periodic, repeating patterns in a [`Sizing`].
    Lookback {
        /// One less than the difference in index between the prior sample and this one.
        ///
        /// For example, if a lookback sample at index `5` has an `index_offset` of `2`, then the
        /// prior sample is at index `2`, and the lookback offset is equal to the difference in
        /// `size` between the lookback sample and the prior sample.
        index_offset: u16,
    },
}

impl Sizing {
    /// Constructs a [`Sizing`] which includes every [`Size2i`].
    pub fn any() -> Self {
        Self::from_samples([Sample {
            size: size2i(0, 0),
            ty: SampleType::Include,
        }])
    }

    /// Constructs an empty [`Sizing`].
    pub fn none() -> Self {
        Self::from_samples([])
    }

    /// Constructs a [`Sizing`] consisting of just the given [`Size2i`].
    pub fn exact(value: Size2i) -> Self {
        Self::range(value, value)
    }

    /// Constructs a [`Sizing`] consisting of the [`Size2i`] whose components are greater than
    /// or equal to `min`, and less than or equal to `max`.
    pub fn range(min: Size2i, max: Size2i) -> Self {
        if max.x < min.x || max.y < min.y {
            Self::none()
        } else if max.x == u32::MAX {
            if max.y == u32::MAX {
                Self::from_samples([Sample {
                    size: min,
                    ty: SampleType::Include,
                }])
            } else {
                Self::from_samples([
                    Sample {
                        size: min,
                        ty: SampleType::Include,
                    },
                    Sample {
                        size: size2i(min.x, max.y + 1),
                        ty: SampleType::Exclude,
                    },
                ])
            }
        } else if max.y == u32::MAX {
            Self::from_samples([
                Sample {
                    size: min,
                    ty: SampleType::Include,
                },
                Sample {
                    size: size2i(max.x + 1, min.y),
                    ty: SampleType::Exclude,
                },
            ])
        } else {
            Self::from_samples([
                Sample {
                    size: min,
                    ty: SampleType::Include,
                },
                Sample {
                    size: size2i(max.x + 1, min.y),
                    ty: SampleType::Exclude,
                },
                Sample {
                    size: size2i(min.x, max.y + 1),
                    ty: SampleType::Exclude,
                },
            ])
        }
    }

    /// Constructs a [`Sizing`] from the given, ordered list of [`Sample`]s.
    fn from_samples<const N: usize>(samples: [Sample; N]) -> Self {
        Self {
            samples: Box::new(samples),
        }
    }

    /// Determines whether this [`Sizing`] contains the given [`Size2i`].
    pub fn contains(&self, value: Size2i) -> bool {
        contains(before_samples(&self.samples, value), value)
    }

    /// Determines whether this [`Sizing`] can be constructed using [`Sizing::range`]. If so,
    /// returns the `min` and `max` arguments for it.
    pub fn as_range(&self) -> Option<(Size2i, Size2i)> {
        match &*self.samples {
            [] => Some((size2i(u32::MAX, u32::MAX), size2i(0, 0))),
            [Sample {
                size: min,
                ty: SampleType::Include,
            }] => Some((*min, size2i(u32::MAX, u32::MAX))),
            [Sample {
                size: min,
                ty: SampleType::Include,
            }, Sample {
                size: bound_y,
                ty: SampleType::Exclude,
            }] if min.x == bound_y.x => Some((*min, size2i(u32::MAX, bound_y.y - 1))),
            [Sample {
                size: min,
                ty: SampleType::Include,
            }, Sample {
                size: bound_x,
                ty: SampleType::Exclude,
            }] if min.y == bound_x.y => Some((*min, size2i(bound_x.x - 1, u32::MAX))),
            [Sample {
                size: min,
                ty: SampleType::Include,
            }, Sample {
                size: bound_x,
                ty: SampleType::Exclude,
            }, Sample {
                size: bound_y,
                ty: SampleType::Exclude,
            }] if min.x == bound_y.x && min.y == bound_x.y => {
                Some((*min, size2i(bound_x.x - 1, bound_y.y - 1)))
            }
            _ => None,
        }
    }

    /// Constructs a [`Sizing`] consisting of the [`Size2i`]s that can be obtained by adding a
    /// non-negative integer to the X component of some size in this [`sizing`].
    pub fn extend_x(&self) -> Self {
        // TODO: Real implementation
        if let Some((min, mut max)) = self.as_range() {
            max.x = u32::MAX;
            Self::range(min, max)
        } else {
            todo!()
        }
    }

    /// Constructs a [`Sizing`] consisting of the [`Size2i`]s that can be obtained by adding a
    /// non-negative integer to the Y component of some size in this [`sizing`].
    pub fn extend_y(&self) -> Self {
        // TODO: Real implementation
        if let Some((min, mut max)) = self.as_range() {
            max.y = u32::MAX;
            Self::range(min, max)
        } else {
            todo!()
        }
    }

    /// Constructs a [`Sizing`] consisting of the [`Size2i`]s, `size2i(x, y)`, that minimize `x`.
    ///
    /// i.e. `size2i(x, y)` is in the returned set if it is in this set and `size2i(x + i, y)` is
    /// not for every `i > 0`.
    pub fn minimize_x(&self) -> Self {
        // TODO: Real implementation
        if let Some((min, max)) = self.as_range() {
            Self::range(min, size2i(min.x, max.y))
        } else {
            todo!()
        }
    }

    /// Constructs a [`Sizing`] consisting of the [`Size2i`]s, `size2i(x, y)`, that minimize `y`.
    ///
    /// i.e. `size2i(x, y)` is in the returned set if it is in this set and `size2i(x, y + i)` is
    /// not for every `i > 0`.
    pub fn minimize_y(&self) -> Self {
        // TODO: Real implementation
        if let Some((min, max)) = self.as_range() {
            Self::range(min, size2i(max.x, min.y))
        } else {
            todo!()
        }
    }

    /// Constructs a [`Sizing`] consisting of the [`Size2i`]s, `size2i(x_a + x_b, y)`, such that
    /// `size2i(x_a, y)` is in `a` and `size2i(x_b, y)` is in `b`.
    pub fn stack_x(a: &Self, b: &Self) -> Self {
        // TODO: Real implementation
        if let (Some((min_a, max_a)), Some((min_b, max_b))) = (a.as_range(), b.as_range()) {
            let min = size2i(min_a.x + min_b.x, u32::max(min_a.y, min_b.y));
            let max = size2i(
                u32::saturating_add(max_a.x, max_b.x),
                u32::min(max_a.y, max_b.y),
            );
            Self::range(min, max)
        } else {
            todo!()
        }
    }

    /// Constructs a [`Sizing`] consisting of the [`Size2i`]s, `size2i(x, y_a + y_b)`, such that
    /// `size2i(x, y_a)` is in `a` and `size2i(x, y_b)` is in `b`.
    pub fn stack_y(a: &Self, b: &Self) -> Self {
        // TODO: Real implementation
        if let (Some((min_a, max_a)), Some((min_b, max_b))) = (a.as_range(), b.as_range()) {
            let min = size2i(u32::max(min_a.x, min_b.x), min_a.y + min_b.y);
            let max = size2i(
                u32::min(max_a.x, max_b.x),
                u32::saturating_add(max_a.y, max_b.y),
            );
            Self::range(min, max)
        } else {
            todo!()
        }
    }

    /// Finds the maximum value of `x` less than or equal to `max_x` such that `size2i(x, y)` is
    /// in this [`Sizing`]. Returns [`None`] if no such value exists.
    pub fn upto_x(&self, mut max_x: u32, y: u32) -> Option<u32> {
        loop {
            let size = size2i(max_x, y);
            let (sample, _) = find_effective(before_samples(&self.samples, size), size);
            match sample.ty {
                SampleType::Exclude => {
                    if sample.size.x == 0 {
                        return None;
                    } else {
                        max_x = sample.size.x - 1;
                    }
                }
                SampleType::Include => return Some(max_x),
                SampleType::Lookback { .. } => todo!(),
            }
        }
    }

    /// Finds the maximum value of `y` less than or equal to `max_y` such that `size2i(x, y)` is
    /// in this [`Sizing`]. Returns [`None`] if no such value exists.
    pub fn upto_y(&self, x: u32, mut max_y: u32) -> Option<u32> {
        loop {
            let size = size2i(x, max_y);
            let (sample, _) = find_effective(before_samples(&self.samples, size), size);
            match sample.ty {
                SampleType::Exclude => {
                    if sample.size.y == 0 {
                        return None;
                    } else {
                        max_y = sample.size.y - 1;
                    }
                }
                SampleType::Include => return Some(max_y),
                SampleType::Lookback { .. } => todo!(),
            }
        }
    }

    /// Finds the minimum value of `x` greater than or equal to `min_x` such that `size2i(x, y)` is
    /// in this [`Sizing`]. Returns [`None`] if no such value exists.
    pub fn downto_x(&self, mut min_x: u32, y: u32) -> Option<u32> {
        loop {
            let size = size2i(min_x, y);
            let (sample, after_i) = find_effective(before_samples(&self.samples, size), size);
            match sample.ty {
                SampleType::Exclude => {
                    if let Some(after_sample) = self.samples.get(after_i) {
                        if after_sample.size.y <= y {
                            debug_assert!(min_x < after_sample.size.x);
                            min_x = after_sample.size.x;
                            continue;
                        }
                    }
                    return None;
                }
                SampleType::Include => return Some(min_x),
                SampleType::Lookback { .. } => todo!(),
            }
        }
    }

    /// Finds the minimum value of `y` greater than or equal to `min_y` such that `size2i(x, y)` is
    /// in this [`Sizing`]. Returns [`None`] if no such value exists.
    pub fn downto_y(&self, x: u32, mut min_y: u32) -> Option<u32> {
        'main: loop {
            let size = size2i(x, min_y);
            let (sample, after_i) = find_effective(before_samples(&self.samples, size), size);
            match sample.ty {
                SampleType::Exclude => {
                    for i in after_i..self.samples.len() {
                        let after_sample = &self.samples[i];
                        if after_sample.size.x <= x {
                            min_y = after_sample.size.y;
                            continue 'main;
                        }
                    }
                    return None;
                }
                SampleType::Include => return Some(min_y),
                SampleType::Lookback { .. } => todo!(),
            }
        }
    }
}

/// Gets the sublist of samples that are lexicographically before (or at) a given [`Size2i`].
/// This will always start at the beginning of the samples list.
fn before_samples(samples: &[Sample], value: Size2i) -> &[Sample] {
    match samples.binary_search_by(|s| s.size.cmp(&value)) {
        Ok(i) => &samples[0..(i + 1)],
        Err(i) => &samples[0..i],
    }
}

/// A "virtual" [`Sample`] that applies to [`Size2i`]s for which no other sample is effective.
static ROOT_SAMPLE: Sample = Sample {
    size: size2i(0, 0),
    ty: SampleType::Exclude,
};

/// Given the sublist of samples that are lexicographically before (or at) a given [`Size2i`],
/// determines which of the samples is "effective" for the size, and returns the index of the
/// sample *after* it. If no such sample exists, this will return `(&ROOT_SAMPLE, 0)`.
fn find_effective(before_samples: &[Sample], value: Size2i) -> (&Sample, usize) {
    let mut cur = before_samples.len();
    while cur > 0 {
        cur -= 1;
        let sample = &before_samples[cur];
        if sample.size.x <= value.x {
            return (sample, cur + 1);
        }
    }
    (&ROOT_SAMPLE, 0)
}

/// Determines whether a [`Sizing`] contains a given [`Size2i`], given the sublist of samples
/// that are lexicographically before (or at) it.
fn contains(before_samples: &[Sample], value: Size2i) -> bool {
    let (sample, next) = find_effective(before_samples, value);
    match sample.ty {
        SampleType::Exclude => false,
        SampleType::Include => true,
        SampleType::Lookback { .. } => todo!(),
    }
}

impl AddAssign<Size2i> for Sizing {
    /// Adds the given [`Size2i`] to all sizes in this [`Sizing`].
    fn add_assign(&mut self, rhs: Size2i) {
        for sample in self.samples.iter_mut() {
            sample.size += rhs;
        }
    }
}

impl Add<Size2i> for &Sizing {
    type Output = Sizing;
    fn add(self, rhs: Size2i) -> Self::Output {
        let mut res = self.clone();
        res += rhs;
        res
    }
}

impl BitAnd for &Sizing {
    type Output = Sizing;

    /// Constructs the intersection of two [`Sizing`]s.
    fn bitand(self, rhs: Self) -> Self::Output {
        Sizing::binop(BinaryOp::AND, self, rhs)
    }
}

impl BitOr for &Sizing {
    type Output = Sizing;

    /// Constructs the union of two [`Sizing`]s.
    fn bitor(self, rhs: Self) -> Self::Output {
        Sizing::binop(BinaryOp::OR, self, rhs)
    }
}

/// Identifies one of the 16 possible binary operations on booleans.
struct BinaryOp(u8);

impl BinaryOp {
    pub const AND: Self = Self(0b1000);
    pub const OR: Self = Self(0b1110);
    pub fn eval(&self, a: bool, b: bool) -> bool {
        (self.0 & (1 << (usize::from(a) | usize::from(b) << 1))) > 0
    }
}

#[test]
fn test_binary_op() {
    assert_eq!(BinaryOp::AND.eval(false, false), false);
    assert_eq!(BinaryOp::AND.eval(false, true), false);
    assert_eq!(BinaryOp::AND.eval(true, false), false);
    assert_eq!(BinaryOp::AND.eval(true, true), true);
    assert_eq!(BinaryOp::OR.eval(false, false), false);
    assert_eq!(BinaryOp::OR.eval(false, true), true);
    assert_eq!(BinaryOp::OR.eval(true, false), true);
    assert_eq!(BinaryOp::OR.eval(true, true), true);
}

impl Sizing {
    /// Constructs a [`Sizing`] by applying a binary operation on the membership of [`Size2i`]s
    /// from two source [`Sizing`]s.
    fn binop(op: BinaryOp, a: &Self, b: &Self) -> Self {
        let mut res = SizingBuilder::new();
        let mut a = a.read();
        let mut b = b.read();

        // Build line-by-line, skipping lines where the extrapolation is correct.
        loop {
            let mut upto_y = u32::MAX;
            loop {
                // Set the value of the next unconfirmed size
                debug_assert_eq!(res.head(), a.head());
                debug_assert_eq!(res.head(), b.head());
                res.set(op.eval(a.get(), b.get()));

                // See how far forward we can skip
                let (ext, mut upto_x) = res.extrapolate();
                match ext {
                    BlockDescription::Include | BlockDescription::Exclude => {
                        let (a_upto_x, a_upto_y) = a.same_for();
                        let (b_upto_x, b_upto_y) = b.same_for();
                        upto_x = upto_x.min(a_upto_x);
                        upto_x = upto_x.min(b_upto_x);
                        upto_y = upto_y.min(a_upto_y);
                        upto_y = upto_y.min(b_upto_y);
                    }
                    BlockDescription::Lookback { offset } => todo!(),
                }

                // Advance to next size
                if upto_x < u32::MAX {
                    let head = size2i(upto_x + 1, res.head().y);
                    res.advance(head);
                    a.advance(head);
                    b.advance(head);
                } else {
                    break;
                }
            }

            // Advance to next line
            if upto_y < u32::MAX {
                let head = size2i(0, upto_y + 1);
                res.advance(head);
                a.advance(head);
                b.advance(head);
            } else {
                break;
            }
        }
        res.build()
    }
}

/// A helper interface for decoding the contents of a [`Sizing`], providing the membership of
/// every possible [`Size2i`] in lexicographical order. Of course, there are `2^64` possible
/// sizes, so to make this practical, the reader also provides a means of reading entire "blocks"
/// when a more succinct description is available and acknowledged.
struct SizingReader<'a> {
    samples: &'a [Sample],
    head: Size2i,

    /// The index of the next sample in `samples` which is after `head`.
    next: usize,
}

impl Sizing {
    /// Constructs a [`SizingReader`] to read the contents of this [`Sizing`].
    fn read(&self) -> SizingReader {
        SizingReader {
            samples: &self.samples,
            head: size2i(0, 0),
            next: match &*self.samples {
                [first, ..] if first.size == size2i(0, 0) => 1,
                _ => 0,
            },
        }
    }
}

impl SizingReader<'_> {
    /// Gets the next [`Size2i`] to be read.
    pub fn head(&self) -> Size2i {
        self.head
    }

    /// Gets whether `head` is included in the [`Sizing`]. Note that this does not automatically
    /// advance the reader.
    pub fn get(&self) -> bool {
        contains(&self.samples[0..self.next], self.head)
    }

    /// Advances `head` to the given [`Size2i`], which must be lexicographically at or after the
    /// current `head`.
    pub fn advance(&mut self, to: Size2i) {
        assert!(self.head <= to);
        self.head = to;
        while self.next < self.samples.len() {
            if self.samples[self.next].size <= self.head {
                self.next += 1;
            } else {
                break;
            }
        }
    }

    /// Gets an upper bound on the X and Y component values such that every [`Size2i`] at or after
    /// `head` whose components are less than or equal to these values has the same membership
    /// status as `head`.
    pub fn same_for(&self) -> (u32, u32) {
        let (sample, mut next) = find_effective(&self.samples[0..self.next], self.head);
        if let SampleType::Lookback { .. } = sample.ty {
            todo!()
        }

        // Determine X validity range
        let mut upto_x = u32::MAX;
        while next < self.next {
            upto_x = upto_x.min(self.samples[next].size.x - 1);
            next += 1;
        }
        if self.next < self.samples.len() {
            let mut next_y = self.next;
            let next_sample = &self.samples[self.next];
            if next_sample.size.y == self.head.y {
                upto_x = upto_x.min(next_sample.size.x - 1);
                next_y += 1;
            }

            // Determine Y validity range
            while next_y < self.samples.len() {
                let next_y_sample = &self.samples[next_y];
                if next_y_sample.size.x <= upto_x {
                    return (upto_x, next_y_sample.size.y - 1);
                } else {
                    next_y += 1;
                }
            }
        }
        (upto_x, u32::MAX)
    }
}

/// A helper interface for constructing a [`Sizing`] by incrementally setting the membership
/// of every possible [`Size2i`] in lexicographical order. Of course, there are `2^64` possible
/// sizes, so to make this practical, the builder provides a prediction/extrapolation of later
/// sizes and allows the user to skip "blocks" of sizes which are predicted correctly. Thus,
/// the user only needs to inform the builder of a (hopefully) small number of deviations from
/// predictions instead of every individual membership value.
struct SizingBuilder {
    samples: Vec<Sample>,
    head: Size2i,
}

impl SizingBuilder {
    /// Creates a new [`SizingBuilder`] with `head` starting at `size2i(0, 0)`.
    pub fn new() -> Self {
        Self {
            samples: Vec::new(),
            head: size2i(0, 0),
        }
    }

    /// Gets the first [`Size2i`] whose membership value is still indeterminate.
    pub fn head(&self) -> Size2i {
        self.head
    }

    /// Sets whether `head` will be included in the resulting [`Sizing`]. Note that this does not
    /// automatically advance the builder to the next size. This may be called at most once per
    /// [`Size2i`].
    ///
    /// Calling this may invalidate the extrapolation of [`Size2i`] memberships whose X and Y
    /// components are both at or after `head`.
    pub fn set(&mut self, include: bool) {
        let cur_include = contains(&self.samples, self.head);
        if cur_include != include {
            self.samples.push(Sample {
                size: self.head,
                ty: if include {
                    SampleType::Include
                } else {
                    SampleType::Exclude
                },
            });
        }
    }

    /// Advances `head` to the given [`Size2i`], which must be lexicographically after the
    /// current `head`. This will "confirm" the predicted memberships of the [`Size2i`]s in
    /// between.
    pub fn advance(&mut self, to: Size2i) {
        assert!(self.head <= to);
        self.head = to;
    }

    /// Gets the current extrapolation of the [`Size2i`] memberships whose X component is at or
    /// after `head.x`. Provides the last X component for which this extrapolation is valid.
    ///
    /// If the builder is advanced past these [`Size2i`]s without calling `set` to determine
    /// their membership, the returned extrapolation will be used to determine their membership.
    /// Thus, `set` only needs to be called when the extrapolated membership of a [`Size2i`]
    /// differs from the desired membership.
    pub fn extrapolate(&self) -> (BlockDescription, u32) {
        let (sample, next) = find_effective(&self.samples, self.head);
        (
            match sample.ty {
                SampleType::Exclude => BlockDescription::Exclude,
                SampleType::Include => BlockDescription::Include,
                SampleType::Lookback { .. } => todo!(),
            },
            if next < self.samples.len() {
                self.samples[next].size.x - 1
            } else {
                u32::MAX
            },
        )
    }

    /// Builds the resulting [`Sizing`], using extrapolated membership values for all remaining
    /// [`Size2i`]s after `head`.
    pub fn build(self) -> Sizing {
        Sizing {
            samples: self.samples.into_boxed_slice(),
        }
    }
}

/// Describes the membership of a range of [`Size2i`]s in a [`Sizing`].
enum BlockDescription {
    /// All [`Size2i`]s in the block are included in the [`Sizing`].
    Include,

    /// All [`Size2i`]s in the block are excluded from the [`Sizing`].
    Exclude,

    /// The membership of a [`Size2i`], `size`, in the block, can be determined by checking the
    /// membership of `size - offset`.
    Lookback { offset: Size2i },
}

#[test]
fn test_range_intersection() {
    let a = Sizing::range(size2i(100, 100), size2i(300, 300));
    let b = Sizing::range(size2i(200, 50), size2i(400, 200));
    let c = Sizing::range(size2i(200, 100), size2i(300, 200));
    assert_eq!(&a & &b, c);
}

#[test]
fn test_range_union_1() {
    let a = Sizing::range(size2i(100, 100), size2i(200, 200));
    let b = Sizing::range(size2i(198, 100), size2i(300, 200));
    let c = Sizing::range(size2i(100, 100), size2i(300, 200));
    assert_eq!(&a | &b, c);
}

#[test]
fn test_range_union_2() {
    let a = Sizing::range(size2i(0, 10), size2i(30, 20));
    let b = Sizing::range(size2i(10, 0), size2i(20, 30));
    let c = &a | &b;
    for x in 0..32 {
        for y in 0..32 {
            let size = size2i(x, y);
            assert_eq!(c.contains(size), a.contains(size) | b.contains(size));
        }
    }
}

#[test]
fn test_stack_simple() {
    let a = Sizing::exact(size2i(10, 10));
    let b = Sizing::any();
    let c = Sizing::range(size2i(10, 10), size2i(u32::MAX, 10));
    assert_eq!(Sizing::stack_x(&a, &b), c);
}

#[test]
fn test_extend_simple() {
    assert_eq!(
        Sizing::exact(size2i(10, 10)).extend_x().extend_y(),
        Sizing::range(size2i(10, 10), size2i(u32::MAX, u32::MAX))
    );
}

#[test]
fn test_any_union() {
    assert_eq!(&Sizing::any() | &Sizing::any(), Sizing::any())
}
