use std::num::NonZeroU32;
use std::cell::Cell;
use std::sync::atomic::AtomicU32;

/// A unique identifier within the scope of a single program run.
#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy)]
pub struct Unique {
    block: NonZeroU32,
    item: u32,
}

/// The next unused [`Unique`] block index.
static NEXT_BLOCK: AtomicU32 = AtomicU32::new(1);

thread_local! {
    /// The previously-returned [`Unique`] for the current thread.
    static PREV_UNIQUE: Cell<Unique> = Cell::new(Unique {
        block: unsafe { NonZeroU32::new_unchecked(u32::MAX) },
        item: u32::MAX
    });
}

impl Unique {
    /// Constructs a new [`Unique`], distinct from all existing [`Unique`]s.
    pub fn new() -> Self {
        PREV_UNIQUE.with(|prev_unique| {
            let mut unique = prev_unique.get();
            if let Some(n_item) = unique.item.checked_add(1) {
                unique.item = n_item;
            } else {
                let block = NEXT_BLOCK.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                let block = NonZeroU32::new(block).expect("ran out of blocks");
                unique.block = block;
                unique.item = 0;
            }
            prev_unique.set(unique);
            unique
        })
    }
}

impl Default for Unique {
    fn default() -> Self {
        Self::new()
    }
}

#[test]
fn test_multithreaded() {
    const NUM_THREADS: usize = 100;
    const NUM_ITEMS_PER_THREAD: usize = 100;
    let uniques = (0..NUM_THREADS)
        .map(|_| {
            std::thread::spawn(|| {
                let mut uniques = Vec::new();
                for _ in 0..NUM_ITEMS_PER_THREAD {
                    uniques.push(std::hint::black_box(Unique::new()));
                }
                uniques
            })
        })
        .collect::<Vec<_>>();
    let uniques = uniques
        .into_iter()
        .flat_map(|h| h.join().unwrap())
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(uniques.len(), NUM_THREADS * NUM_ITEMS_PER_THREAD);
}
