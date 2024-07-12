use super::*;
use ghost_cell::{GhostCell, GhostToken};
use rand::prelude::*;
use std::cell::Cell;
use std::num::NonZeroU64;

/// An implementation of [`React`] and [`Track`] based on [`GhostToken`] and [`Version`].
pub struct VersionReact<'brand> {
    token: GhostToken<'brand>,
    version: Version,
    validity: Cell<Version>,
}

impl VersionReact<'_> {
    /// Constructs a new [`VersionReact`] with the `'static` brand.
    ///
    /// ## Safety
    /// It is not safe to mix [`ReactCell`]s between different [`VersionReact`]s. Usually this
    /// would be checked by generating a unique `'brand` for each [`VersionReact`] and tying the
    /// [`ReactCell`] to that brand. However, this function allows multiple [`VersionReact`]s
    /// to be created with the same `'brand`. Thus, it is up to the caller to ensure that
    /// [`ReactCell`] are not mixed between [`VersionReact`]s.
    pub unsafe fn new_static() -> VersionReact<'static> {
        VersionReact {
            token: unsafe { std::mem::zeroed() },
            version: Version::MIN,
            validity: Cell::new(Version::MAX),
        }
    }
}

impl<'brand> React for VersionReact<'brand> {
    type RawCell<T> = (GhostCell<'brand, T>, Aspect);

    fn new_cell<T>(&self, value: T) -> ReactCell<Self, T> {
        ReactCell::from_raw((GhostCell::new(value), Aspect::new()))
    }

    fn with_cell_ref<T, R>(&self, cell: &ReactCell<Self, T>, f: impl FnOnce(&T) -> R) -> R {
        let (cell, aspect) = cell.as_raw();
        let mut validity = self.validity.get();
        validity.reduce_to_for(&self.version, *aspect);
        self.validity.set(validity);
        f(cell.borrow(&self.token))
    }

    fn with_cell_mut<T, R>(&mut self, cell: &ReactCell<Self, T>, f: impl FnOnce(&mut T) -> R) -> R {
        let (cell, aspect) = cell.as_raw();
        self.version.incr(*aspect);
        self.validity.set(Version::MIN); // Validity token is immediately invalidated
        f(cell.borrow_mut(&mut self.token))
    }

    fn set_cell<T>(&mut self, cell: &ReactCell<Self, T>, value: T) {
        // Unlike `with_cell_mut`, the validity token is not invalidated since the
        // value is not read.
        let (cell, aspect) = cell.as_raw();
        self.version.incr(*aspect);
        *cell.borrow_mut(&mut self.token) = value;
    }
}

impl<'brand> Track for VersionReact<'brand> {
    type ValidityToken = Version;

    fn track<R>(&self, inner: impl FnOnce() -> R) -> (R, Version) {
        let mut cur_validity = self.validity.get();
        self.validity.set(Version::MAX);
        let res = inner();
        let res_validity = self.validity.get();
        cur_validity.reduce_to(&res_validity);
        self.validity.set(cur_validity);
        (res, res_validity)
    }

    fn is_valid(&self, token: &Version) -> bool {
        if self.version.fast_any_gt(token) {
            false
        } else {
            let mut validity = self.validity.get();
            validity.reduce_to(token);
            self.validity.set(validity);
            true
        }
    }
}

/// Identifies the version for a [`VersionReact`]. This is not merely a number, but a sort of
/// [version vector](https://en.wikipedia.org/wiki/Version_vector) over several [`Aspect`]s.
/// Incrementing the version for one aspect usually will not affect the version from the
/// perspective of another aspect.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Version {
    l_0: u64,        // 64 groups of 1 bit
    l_1: u64,        // 16 groups of 4 bits
    l_2: NonZeroU64, // 1 group of 64 bits
}

impl Version {
    /// The least version.
    pub const MIN: Self = Self {
        l_0: 0,
        l_1: 0,
        l_2: unsafe { NonZeroU64::new_unchecked(1) },
    };

    /// The greatest version.
    pub const MAX: Self = Self {
        l_0: u64::MAX,
        l_1: u64::MAX,
        l_2: unsafe { NonZeroU64::new_unchecked(u64::MAX) },
    };

    /// Gets the least version which is greater than this version in the given aspect.
    pub fn incr(&mut self, aspect: Aspect) {
        let l_0_offset = aspect.0;
        let l_0 = ((self.l_0 >> l_0_offset) & 1) + 1;
        if l_0 <= 1 {
            self.l_0 = (self.l_0 & !(1 << l_0_offset)) | (l_0 << l_0_offset);
        } else {
            let l_1_offset = aspect.0 / 4 * 4;
            let l_1 = ((self.l_1 >> l_1_offset) & 0b1111) + 1;
            if l_1 <= 0b1111 {
                self.l_0 &= !(0b1111 << l_1_offset);
                self.l_1 = (self.l_1 & !(0b1111 << l_1_offset)) | (l_1 << l_1_offset);
            } else {
                self.l_0 = 0;
                self.l_1 = 0;
                self.l_2 = self.l_2.checked_add(1).expect("Maximum version reached");
            }
        }
    }

    /// Reduces this version so that it is not greater than the given version in any aspect.
    pub fn reduce_to(&mut self, other: &Version) {
        self.l_0 &= other.l_0;
        self.l_1 &= other.l_1; // TODO: We should take the min of each group. This is a loose approximation
        self.l_2 = self.l_2.min(other.l_2);
    }

    /// Reduces this version so that it is not greater than the given version in the given aspect.
    fn reduce_to_for(&mut self, other: &Version, aspect: Aspect) {
        let l_0_mask = 1 << aspect.0;
        self.l_0 &= !l_0_mask | other.l_0;
        let l_1_mask = 0b1111 << (aspect.0 / 4 * 4);
        self.l_1 &= !l_1_mask | other.l_1;
        self.l_2 = self.l_2.min(other.l_2);
    }

    /// Determines whether any aspect in this version is greater than the corresponding aspect
    /// in the given version. This assumes that for each aspect, either this version is greater
    /// than or equal to the given version, or the given version has the maximum value for that
    /// aspect.
    fn fast_any_gt(&self, other: &Version) -> bool {
        ((self.l_0 & !other.l_0) != 0) | ((self.l_1 & !other.l_1) != 0) | (self.l_2 > other.l_2)
    }
}

impl Default for Version {
    fn default() -> Self {
        Self::MIN
    }
}

/// Identifies one "aspect" of a [`VersionReact`] that can be tracked in a [`Version`].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Aspect(u8);

impl Aspect {
    /// Generates an [`Aspect`]. The number of possible [`Aspect`]s are limited, so this will try
    /// to distribute aspects to minimize collisions and improve performance.
    pub fn new() -> Self {
        rand::random()
    }
}

impl Default for Aspect {
    fn default() -> Self {
        Self::new()
    }
}

impl Distribution<Aspect> for rand::distributions::Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Aspect {
        Aspect((rng.next_u32() % 64) as u8)
    }
}

#[test]
fn test_version() {
    let mut rng: StdRng = SeedableRng::seed_from_u64(1);
    let mut version = Version::MIN;
    let mut true_positives = 0;
    let mut false_positives = 0;
    for _ in 0..1000 {
        // Observe a random set of aspects
        let mut validity = Version::MAX;
        let mut observed = 0u64;
        for _ in 0..rng.gen_range(1..10) {
            let aspect = Aspect::new();
            observed |= 1 << aspect.0;
            validity.reduce_to_for(&version, aspect);
        }

        // Update a random set of aspects
        let mut updated = 0u64;
        for _ in 0..rng.gen_range(1..10) {
            let aspect = Aspect::new();
            updated |= 1 << aspect.0;
            version.incr(aspect);
        }

        // Check whether the change is detected
        let change_exists = (observed & updated) != 0;
        let change_detected = version.fast_any_gt(&validity);
        if change_exists {
            assert!(change_detected);
            true_positives += 1;
        } else if change_detected {
            false_positives += 1;
        }
    }

    // Put an arbitrary bound on number of false positives for testing purposes
    println!("Number of true positives: {:?}", true_positives);
    println!("Number of false positives: {:?}", false_positives);
    assert!(false_positives <= 200);
}
