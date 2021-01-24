//! The context with which a compilation is performed.

use crate::sfnt::tables::cmap::EncodingRecord;

/// A context defines customization options.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Context {
    /// The encoding records used by [`sfnt::tables::cmap`](crate::sfnt::tables::cmap).
    ///
    /// According to the [TrueType specification][spec], encoding records must be sorted first in ascending order of platform id and second by encoding id (a.k.a. platform-specific id). `EncodingRecord` implements this ordering for both `PartialOrd` and `Ord`, so the default sorting of a vector results in the required order.
    ///
    /// [spec]: https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cmap.html
    pub cmap_encoding_records: Option<Vec<EncodingRecord>>,
    /// Whether to use advanced segment merging techniques in [`sfnt::tables::cmap::format4`](crate::sfnt::tables::cmap::format4).
    pub cmap_format4_advanced_segment_merging: bool,
    /// Whether to evaluate all segments or evaluate a limited selection of segments in [`sfnt::tables::cmap::format4`](crate::sfnt::tables::cmap::format4).
    pub cmap_format4_evaluate_all_segments: bool,
}

impl Context {
    /// Creates a context for an abstract optimization level.
    pub fn new(o: Optimization) -> Self {
        Context {
            cmap_encoding_records: None,
            cmap_format4_advanced_segment_merging: o >= Optimization::O1,
            cmap_format4_evaluate_all_segments: o >= Optimization::O2,
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Context::new(Optimization::O0)
    }
}

/// An abstract optimization level.
///
/// The optimization applies to the size of the compiled artifacts.
///
/// The levels are ordered by increasing optimization.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Optimization {
    /// Minimal optimization.
    ///
    /// The optimizations that are applied at this level are considered foundational.
    /// The compilation is as fast as possible at the expense of any attribute that could be optimized.
    ///
    /// This optimization level is useful during development.
    O0,
    /// Basic optimization.
    ///
    /// The optimizations that are applied at this level are considered to be expected by any non-minimal optimization.
    /// Only basic optimizations are applied in addition to the ones of `O0` which makes this level comparable in compilation speed to `O0` but results in better-optimized artifacts.
    ///
    /// This optimization level is useful when fast compilation is desired outside of development.
    O1,
    /// Advanced optimization.
    ///
    /// The optimizations that are applied at this level are considered production-ready.
    /// Advanced optimizations are applied which may take some time to compute.
    /// Input of any scope compiles in a reasonable time.
    ///
    /// This optimization level is useful for the distribution of artifacts.
    O2,
    /// Maximal optimization.
    ///
    /// The optimizations that are applied at this level are considered perfectionistic.
    /// The algorithms used at this optimization level are either expensive approximations or optimal implementations which may use considerable time and memory.
    /// The compilation may not terminate in a reasonable time.
    ///
    /// This optimization level is useful for input that is sufficiently small in scope.
    O3,
}

impl Optimization {
    /// Returns the optimization for the given name, or `None` if the name is invalid.
    pub fn from_name(name: &str) -> Option<Optimization> {
        match name {
            "O0" => Some(Optimization::O0),
            "O1" => Some(Optimization::O1),
            "O2" => Some(Optimization::O2),
            "O3" => Some(Optimization::O3),
            _ => None,
        }
    }
}

impl Default for Optimization {
    fn default() -> Self {
        Optimization::O0
    }
}
