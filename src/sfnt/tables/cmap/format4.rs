//! # Format 4: Segment mapping to delta values
//!
//! Implementation of the `cmap` format 4 subtable.
//!
//! *Specification:*
//! [OpenType](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values),
//! [TrueType](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cmap.html).
//!
//! The entry to this module is the [`compile`] function.
//! It returns a `cmap` format 4 subtable for a given character map.
//!
//! The `cmap` format 4 subtable allows for considerable file size optimizations.
//! Some of these optimizations are enabled by default, others need to be enabled manually.
//! The compiler feature [`cmap_format4_advanced_segment_merging`](Context::cmap_format4_advanced_segment_merging) controls whether advanced optimizations should be applied and [`cmap_format4_evaluate_all_segments`](Context::cmap_format4_evaluate_all_segments) allows for further optimizations.
//!
//! ## Examples
//!
//! ```
//! # use informa::ctx::Context;
//! # use informa::data::CharacterMap;
//! # use informa::sfnt::tables::cmap::format4::compile;
//! let ctx = Context::default();
//! let mut map = CharacterMap::new();
//! map.insert('A', 1);
//! map.insert('B', 2);
//! map.insert('C', 3);
//! let subtable = compile(&map, &ctx);
//! ```
//!
//! [`compile_segments`] allows manual control over the segments used.
//! [`encoding_segments`] returns a segmentation for a character map.
//!
//! ```
//! # use informa::ctx::Context;
//! # use informa::data::CharacterMap;
//! # use informa::sfnt::tables::cmap::format4::encoding_segments;
//! # use informa::sfnt::tables::cmap::format4::compile_segments;
//! # use informa::sfnt::tables::cmap::format4::Segment;
//! # let ctx = Context::default();
//! # let mut map = CharacterMap::new();
//! # map.insert('A', 1);
//! # map.insert('B', 2);
//! # map.insert('C', 3);
//! let mut segments = encoding_segments(&map, &ctx);
//! segments.push(Segment::sentinel());
//! let subtable = compile_segments(&map, &segments);
//! ```

// ## Introduction to Format 4 Segmentation
//
// The `cmap` format 4 subtable maps character codes (*codes*) to glyph ids (*gids*).
// Storing a code → gid mapping for each character supported by the font would consume a lot of storage space.
// A format 4 subtable instead stores *segments* that map a range of codes to a range of gids.
//
// A segment consists of four numbers:
// 1. The start of the character code range.
// 2. The end of the character code range.
// 3. An idDelta.
// 4. An idRangeOffset.
//
// The range of a segment must be *continuous*, meaning that all characters from the start to the end of the segment must be supported by the font.
// For example, a font supporting the characters [A, B, C, X, Y, Z] needs at least two segments: one from A to C and one from X to Z.
//
// The start and end values define the character code range of a segment.
// The mapping to glyph ids can be encoded by using one of two strategies:
//
// The *shift* strategy shifts the character code range by a fixed value to the glyph id range.
// This strategy can only be used if the glyph ids are also continuous.
// For example, the characters [A, B, C] (represented as the codepoints [`0x41`, `0x42`, `0x43`]) can be mapped to the glyph ids [`0x1`, `0x2`, `0x3`] by shifting each character code by `-0x40`.
// This shift value is stored in the idDelta field and the idRangeOffset field is set to `0`.
//
// The *mix* strategy can encode non-continuous glyph ids.
// For example, [A, B, C] mapping to [`0x3`, `0x2`, `0x1`] can not be encoded using a single shift segment since shifting by a fixed idDelta can not change the order.
// Instead, three shift segments would be required; shifting A by `-0x3E`, B by `-0x40`, and C by `-0x42`.
// A mix segment, however, can map a continuous range of character codes to arbitrary glyph ids.
// It does this by storing the target glyph ids in a separate storage (glyphIdArray) and uses idRangeOffset to point to that storage.
// While this strategy requires additional storage space for every code → gid mapping, the size of a single mix segment can be smaller than that of multiple shift segments.
//
// ## On Format 4 Compression
//
// The interplay of shift and mix segments is what makes `cmap` format 4 encoding interesting.
// A character map mapping codes to gids could be encoded using purely shift segments, purely mix segments, just a single mix segment, and any combination of shift and mix segments in between.
//
// ### Introduction to Format 4 Compression
//
// *Note:* For the sake of simplicity, the following examples assume A has the character code 1, B has 2, and so on.
// In most real-world character encodings (e.g. ASCII and Unicode), A has the character code 65, B has 66, and so on.
//
// Consider the following character map mapping characters to glyph ids:
//
// ```text
// A → 2
// B → 1
// C → 3
// D → 4
// E → 5
// ```
//
// This map can be encoded using a shift segment for each mapping:
//
// ```text
// A--A → -1
// B--B →  1
// C--C →  0
// D--D →  0
// E--E →  0
// ```
//
// The `$start--$end` denotes the start and end characters of a segment and the `→` points to its idDelta value.
//
// Using a shift segment for each mapping wastes a lot of space.
// The map can instead be encoded using only three shift segments:
//
// ```text
// A--A → -1
// B--B →  1
// C--E →  0
// ```
//
// The map can also be encoded using a single mix segment:
//
// ```text
// A--E → [2, 1, 3, 4, 5]
// ```
//
// Here, idDelta is `0` and `→` points to glyphIdArray, a storage space separate from the segment where the glyph id for each character is stored.
// The pointer to the storage is stored in the idRangeOffset field of the segment.
//
// Shift and mix segments can be combined.
// For example, the map can be encoded using one mix and one shift segment:
//
// ```text
// A--B → [2, 1]
// C--E → 0
// ```
//
// The interest in these different segmentations lies in the storage space savings from picking one segmentation over another.
// Put simply: A better segmentation reduces the file size of the font.
//
// For any character map exists one or multiple *optimal* segmentations.
// Such an optimal segmentation can easily be found by constructing all possible segmentations, computing their size requirement, and picking the segmentation with the smallest size.
// This brute-force algorithm is only possible for small numbers of mappings (*n*) since it has order 2^*n* time complexity.
//
// The algorithm used by this module approximates the optimal segmentation.
// For many mappings, this approximation returns the optimal result.
// The test `test_encoding_segments_brute_force_all_permutations` ensures that all possible mappings from length 1 to length 7 are encoded optimally.
// In fact, the test can be modified and has successfully tested all mappings up to the length of 10.
// Since running the test in that configuration takes an awfully long time to complete, only the abridged version is included in the standard test suite.
//
// ### How the Algorithm of This Module Works
//
// [`encoding_segments`] is the entry point into this module.
// It takes a character map and returns a vector of segments.
// This vector is the best segmentation that the algorithm was able to find.
//
// Since a segment requires its character rage to be continuous, [`encoding_segments`] must first split the mappings provided by the character map into groups of continuous character ranges.
// This initial grouping is trivial and runs in *O*(*n*).
//
// Each group is passed to [`encoding_segments_continuous`].
// This is the core of the algorithm.
//
// #### Stage 1
//
// Firstly, for each run of continuous glyph ids, a shift segment is formed.
// This is computed in *O*(*n*) time.
// For example, for the glyph ids\* [`1`, `2`, `3`, `4`, `6`, `5`, `7`] the following segments are formed:
//
// ```text
// 1,2,3,4 | 6 | 5 | 7,8
// [32 = 8 + 8 + 8 + 8] (OOOO)
// ```
//
// The notation above is used by the debugging logging of the module.
// (Enable `trace`-level logging to see this debugging output.)
// The first line displays the segmentation:
// Currently, there are four segments; one with four mappings, two segments with one mapping, and one segment with two mappings.
// The second line shows the overall size in bytes of the overall segmentation as well as the sizes of the individual segments.
// The four *O* in parentheses indicate that all segments are ordered (and therefore use the shift strategy).
//
// This is the result of the first stage: ordered segments.
//
// \* While the mappings managed by segments consist of a character code and a glyph id, only the glyph id is of interest to [`encoding_segments_continuous`].
// This is because the character codes of all input mappings are continuous; their relative position is the same for every possible input to the algorithm, unlike assigned the glyph ids.
//
// #### Stage 2
//
// Secondly, all segments with a single mapping are merged.
//
// ```text
// 1,2,3,4 | 6,5 | _ | 7,8
// [28 = 8 + 12 + 8] (OX_O)
// ```
//
// This makes sense since a single mix segment will always be smaller than multiple single-mapping shift segments.
// The size calculation above illustrates this:
// Individually, `5` and `6` cost as shift segments 8 bytes each.
// Merged into a mix segment, the size is reduced to 12 bytes down from 16.
// The `_` indicates a discarded segment.
// It once managed the mapping with glyph id 6 but has since been merged into another segment.
// To prevent moving segment in memory whenever a segment has been merged into another segment it is not deleted but just marked as discarded.
// The strategies-indicator now also features an *X* indicating a mix segment at index 1.
// Stage two also runs in *O*(*n*) time.
//
// #### Stage 3
//
// Thirdly, the shortest segment is merged with its neighbors until no better segmentation can be achieved.
// Merging from the shortest segment is a good approximation; since a short segment manages few mappings the overhead of storing an additional segment is greater in proportion than for a long segment.
//
// Evaluating *all* segments (not just the shortest) improves the approximation, but this mode requires more computation and is deactivated by default.
// If desired, the mode can be activated by enabling the `cmap-format4-evaluate-all-segments` compiler flag.
//
// The shortest segments (there might be multiple segments with the same shortest length) are determined in *O*(*n*) time.
// For each shortest segment, all possible merges to the left and the right are evaluated.
// Note that this does *not* include all possible merges; only the best left merge is combined with the best right merge.
//
// ```text
//                                      ↓ shortest segment
//  indices:   0     1       2    3     4   5      6
// segments: [ ... | ..... | .. | ... | . | .... | ... ]
//            \_____\_______\____\_______/ ← evaluate left merges
//             evaluate right merges → \________/_____/
// ```
//
// Whether a string of mix and shift segments should be merged is decided by comparing the size of a single merged mix segment with the sum of the individual segment sizes.
// These functions compute the sizes for a segment with `n` mappings:
//
// ```no_run
// let shift_segment_size = |n: usize| 8;
// let mix_segment_size   = |n: usize| 8 + n * 2;
// ```
//
// Note that the size of a shift segment is independent of the number of mappings it manages.
// A mix segment is always larger than a shift segment.
// Only because a single mix segment can replace *multiple* shift segments is it an effective compression strategy.
// The function [`Segment::size_of_mix_segment`] is provided internally by the module; the constant `SEGMENT_CORE_SIZE` can be used for shift segments.
//
// In the example from above, the segments at index 1 and 3 are the shortest.
// The segment at index 1 (`6,5`) saves zero bytes when merging to the left.
// The algorithm considers this an improved segmentation even though the file size is not reduced.
// This is because merging, even if not needed now, allows the algorithm to find more merge opportunities in future iterations since mix segments merge much more easily than shift segments.
// Merging two mix segments always reduces the file size since the overhead of one segment is avoided.
// This is also why the algorithm never produces neighboring mix segments.
//
// If multiple segments share the shortest length; all segments are evaluated and only the best match is selected and merged.
// The internal `evaluate_merging_segments` finds the best merge.
//
// Continuing the example from above, this is the result of the third stage:
//
// ```text
// _ | 1,2,3,4,6,5,7,8 | _ | _
// [24 = 24] (_X__)
// ```
//
// All segments have been merged into the segment at index 1 with a final size of 24 bytes.
//
// The third stage is computed in *O*(*n*^3) time where *n* is typically in the range 1–30.
// While *n*^3 appears to be unacceptably slow with *n* possibly reaching 100, 500, or even 60 000, the limit is *n* log(*n*)^2 in practice.
// With the `cmap-format4-evaluate-all-segments` compiler flag enabled, the third stage is computed in *O*(*n*^3) time with a limit of *n*^2 log(*n*) in practice.
// Disabling the `cmap_format4_advanced_segment_merging` compiler flag skips stage 3.
//
// ### Non-Optimal Segmentations
//
// There are inputs for which this module does not produce an optimal segmentation.
// By default, only basic optimizations are applied (stage 2, but not stage 3).
// With the `cmap_format4_advanced_segment_merging` compiler flag set, stage 3 optimizations are applied.
// Stage 3, however, is still just an approximation of the optimal segmentation.
// Consider the following glyph id input sequence.
//
// ```text
// 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 18
// ```
//
// Here, stage 1 and 2 produce the following intermediate segmentation.
//
// ```text
// 2 | 4,5,6,7,8,9,10,11 | 13,15 | _ | 17,18
// [36 = 8 + 8 + 12 + 8] (OOX_O)
// ```
//
// By evaluating all merges from the shortest segment (at index 0), merging all segments is found to result in the smallest size of 34 bytes:
//
// ```text
// 2,4,5,6,7,8,9,10,11,13,15,17,18
// [34 = 34] (X)
// ```
//
// The optimal segmentation, however, would be to merge the last two segments and keep the remaining segments.
// This results in a total size of just 32 bytes.
//
// ```text
// 2 | 4,5,6,7,8,9,10,11 | 13,15,17,18
// [32 = 8 + 8 + 16] (OOX)
// ```
//
// The heuristic that the shortest segments should be merged first does not apply in this case.
//
// This module provides a compiler flag, `cmap_format4_evaluate_all_segments`, which evaluates all segments independent of their size.
// This optimization finds the optimal segmentation for the example above.
//
// Note that this is still an approximation and that there are inputs for which a non-optimal segmentation is returned.
// The following shows such an input.
//
// ```text
// 2, 3, 4, 6, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 23, 24, 26, 28, 29, 30
// ```
//
// Here, the following intermediate segmentation is produced by stage 1 and stage 2.
//
// ```text
// 2,3,4 | 6 | 8,9 | 11,12,13,14,15,16,17,18,19 | 21 | 23,24 | 26 | 28,29,30
// [64 = 8 + 8 + 8 + 8 + 8 + 8 + 8 + 8] (OOOOOOOO)
// ```
//
// Since stage 3 operates greedily, it can not find the optimal segmentation of:
//
// ```text
// 2,3,4,6,8,9 | 11,12,13,14,15,16,17,18,19 | 21,23,24,26,28,29,30
// [50 = 20 + 8 + 22] (XOX)
// ```
//
// Instead, all segments are merged by the algorithm, requiring two bytes more than the optimal segmentation.
//
// ```text
// 2,3,4,6,8,9,11,12,13,14,15,16,17,18,19,21,23,24,26,28,29,30
// [52 = 52] (X)
// ```
//
// Even when evaluating all segments, the optimal segmentation is not found by this module.
// This is because evaluating a segment compares all possible left and all possible right merges and combines the best left with the best right merge.
// In the case of the example above, however, this greedy approach prevents the algorithm from considering merging the left and the right sides and leaving the middle unchanged.
//
// ### Improving the Algorithm
//
// The format 4 segmentation algorithm as provided by this module is not optimal.
// To test future improvements, have a look at the tests (`test_encoding_segments_brute_force_notorious` and `test_encoding_segments_brute_force_random` in particular).
// To quickly check the size of a segmentation, use the tool provided at <https://addpixel.net/informa/cmap-format4-segmentation-test>.

use crate::ctx::Context;
use crate::data::CharacterMap;
use crate::util::slice::{split_pivot, split_pivot_mut};
use bytes::{BufMut, Bytes, BytesMut};
use itertools::Itertools;
use spans::Spans;
use std::mem::size_of;

/// The Unicode scaler used as a sentinel value for the character search.
///
/// Quoting the [OpenType specification][spec]:
///
/// > For the search to terminate, the final start code and endCode values must be 0xFFFF.
///
/// [spec]: https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values
pub const SENTINEL_SCALER: char = '\u{FFFF}';

/// The size in bytes of the `format` field.
const FORMAT_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `length` field.
const LENGTH_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `language` field.
const LANGUAGE_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `segCountX2` field.
const SEG_COUNT_X2_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `searchRange` field.
const SEARCH_RANGE_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `entrySelector` field.
const ENTRY_SELECTOR_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `rangeShift` field.
const RANGE_SHIFT_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of an `endCode` field.
const END_CODE_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `reservedPad` field.
const RESERVED_PAD_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of a `startCode` field.
const START_CODE_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of an `idDelta` field.
const ID_DELTA_FIELD_SIZE: usize = size_of::<i16>();
/// The size in bytes of an `idRangeOffset` field.
const ID_RANGE_OFFSET_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of a `glyphIdArray` field.
const GLYPH_ID_ARRAY_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the subtable header and `reservedPad`.
const CONSTANT_SIZE: usize = FORMAT_FIELD_SIZE
    + LENGTH_FIELD_SIZE
    + LANGUAGE_FIELD_SIZE
    + SEG_COUNT_X2_FIELD_SIZE
    + SEARCH_RANGE_FIELD_SIZE
    + ENTRY_SELECTOR_FIELD_SIZE
    + RANGE_SHIFT_FIELD_SIZE
    + RESERVED_PAD_FIELD_SIZE;
/// The size in bytes of a single segment, excluding the additional size which may be required for `glyphIdArray`.
const SEGMENT_CORE_SIZE: usize =
    END_CODE_FIELD_SIZE + START_CODE_FIELD_SIZE + ID_DELTA_FIELD_SIZE + ID_RANGE_OFFSET_FIELD_SIZE;

/// Returns a `cmap` format 4 subtable representing the given character map.
///
/// # Example
///
/// ```
/// # use informa::ctx::Context;
/// # use informa::data::CharacterMap;
/// # use informa::sfnt::tables::cmap::format4::compile;
/// let ctx = Context::default();
/// let mut map = CharacterMap::new();
/// # map.insert('A', 1);
/// # map.insert('B', 2);
/// # map.insert('C', 3);
/// // ... insert entries into character map ...
/// let subtable = compile(&map, &ctx);
/// ```
pub fn compile(map: &CharacterMap, ctx: &Context) -> Bytes {
    let mut segments = encoding_segments(&map, ctx);
    segments.push(Segment::sentinel());

    compile_segments(&map, &segments)
}

/// Returns a `cmap` format 4 subtable representing the given segments.
///
/// Note that this function does not automatically append a sentinel segment at the end of the segments vector.
/// If such a sentinel segment is desired it must be provided as the last element of `segments`.
///
/// # Example
///
/// ```
/// # use informa::ctx::Context;
/// # use informa::data::CharacterMap;
/// # use informa::sfnt::tables::cmap::format4::compile_segments;
/// # use informa::sfnt::tables::cmap::format4::encoding_segments;
/// # use informa::sfnt::tables::cmap::format4::Segment;
/// let ctx = Context::default();
/// let mut map = CharacterMap::new();
/// // ... insert entries into character map ...
/// # map.insert('A', 1);
/// # map.insert('B', 2);
/// # map.insert('C', 3);
/// let mut segments = encoding_segments(&map, &ctx);
/// segments.push(Segment::sentinel());
///
/// let subtable = compile_segments(&map, &segments);
/// ```
pub fn compile_segments(map: &CharacterMap, segments: &[Segment]) -> Bytes {
    let length = CONSTANT_SIZE + segments.iter().map(Segment::size).sum::<usize>();
    let mut buf = BytesMut::with_capacity(length);

    let format = 4;
    buf.put_u16(format);

    buf.put_u16(length as u16);

    let language = 0;
    buf.put_u16(language);

    let seg_count = segments.len() as u16;

    let seg_count_x2 = seg_count * 2;
    buf.put_u16(seg_count_x2);

    let search_range: u16 = 2 * 2u16.pow((seg_count as f64).log2().floor() as u32);
    buf.put_u16(search_range);

    let entry_selector = ((search_range as f64) / 2f64).log2().floor() as u16;
    buf.put_u16(entry_selector);

    let range_shift = seg_count_x2 - search_range;
    buf.put_u16(range_shift);

    // endCode
    for segment in segments {
        buf.put_u16(segment.end as u16);
    }

    let reserved_pad = 0;
    buf.put_u16(reserved_pad);

    // startCode
    for segment in segments {
        buf.put_u16(segment.start as u16);
    }

    // idDelta
    for segment in segments {
        match segment.strategy {
            SegmentEncodingStrategy::Shift => {
                let start_char = segment.start;
                // Map to glyph id `0` (`.notdef`) if no mapping is defined for the character.
                // This is required to correctly map U+FFFF, which is used as a sentinel segment but does not have any mapping defined in `map`.
                // See also [`Segment::sentinel`].
                let start_gid = map.get(&start_char).map_or(0, |&x| x) as i32;
                let start_code = start_char as i32;
                let id_delta = start_gid.wrapping_sub(start_code) as i16;
                buf.put_i16(id_delta);
            }
            SegmentEncodingStrategy::Mix => {
                buf.put_i16(0);
            }
        }
    }

    let mut glyph_id_array_offset: usize = GLYPH_ID_ARRAY_FIELD_SIZE;

    // idRangeOffset
    for (index, segment) in segments.iter().enumerate() {
        match segment.strategy {
            SegmentEncodingStrategy::Shift => {
                buf.put_u16(0);
            }
            SegmentEncodingStrategy::Mix => {
                // buf:
                //
                //      index                                   id_range_offset
                //        ↓                                            ↓
                // ...[P][S][ ][ ][ ][ ][ ][ ][A][A][A][A][A][A][A][A][ ][ ]...
                // \_________________________/\_________________________/
                //     idRangeOffset block       glyph_id_array_offset
                //          \________________/\_______________________________/
                //      remaining_id_range_offset    glyphIdArray block
                //
                //     P = preceding `segment`
                //     S = current `segment`
                //     A = glyphIdArray field reserved by a preceding segment
                //         (just reserved, set in `glyphIdArray` loop below)

                let remaining_id_range_offset =
                    (segments.len() - index - 1) * ID_RANGE_OFFSET_FIELD_SIZE;
                let id_range_offset = (glyph_id_array_offset + remaining_id_range_offset) as u16;
                buf.put_u16(id_range_offset);

                glyph_id_array_offset += segment.len() * GLYPH_ID_ARRAY_FIELD_SIZE;
            }
        }
    }

    // glyphIdArray
    for segment in segments
        .iter()
        .filter(|x| x.strategy == SegmentEncodingStrategy::Mix)
    {
        for code in segment.start..=segment.end {
            // `0` is used as a default value for the same reason as outlined in the `idDelta` loop above
            let gid = map.get(&code).map_or(0, |x| *x) as u16;
            buf.put_u16(gid);
        }
    }

    assert_eq!(length, buf.len());

    buf.freeze()
}

/// Returns a debug description of a slice of segments.
///
/// This function is **not optimized for runtime performance or memory efficiency**.
/// Use it for debugging only.
///
/// The format is a follows:
///
/// ```text
/// 1,2,3 | 6,5,4 | _ | 8 | 10,9
/// [42 = 8 + 14 + _ + 8 + 12] (OX_OX)
/// ```
///
/// Segments are separated by a vertical bar (`|`) and the glyph ids of each segment are comma-separated.
/// `None` values are displayed as an underscore (`_`).
/// The size in bytes of the segmentation as well as the individual sizes of each segment are set in square brackets.
/// The strategies are set in parentheses: `O` indicates the *shift* strategy; `X` indicates the *mix* segment.
///
/// The `map` parameter is used to resolve the glyph ids for each segment.
fn debug_segments_description(map: &CharacterMap, segments: &[Option<Segment>]) -> String {
    let segments_desc = segments
        .iter()
        .map(|x| {
            if let Some(segment) = x {
                (segment.start..=segment.end)
                    .map(|x| format!("{}", map[&x]))
                    .join(",")
            } else {
                "_".to_string()
            }
        })
        .collect::<Vec<String>>()
        .join(" | ");
    let strategies_desc = segments
        .iter()
        .map(|x| {
            if let Some(segment) = x {
                format!("{:?}", segment.strategy)
            } else {
                "_".to_string()
            }
        })
        .collect::<Vec<String>>()
        .join("");
    let sizes = segments
        .iter()
        .flatten()
        .map(Segment::size)
        .collect::<Vec<usize>>();
    let sizes_desc = sizes
        .iter()
        .map(|&x| {
            if x != 0 {
                format!("{}", x)
            } else {
                "_".to_string()
            }
        })
        .collect::<Vec<String>>()
        .join(" + ");
    let size_sum: usize = sizes.iter().sum();

    format!(
        "{}\n[{} = {}] ({})",
        segments_desc, size_sum, sizes_desc, strategies_desc
    )
}

/// Returns a segmentation of the given character map suitable for [`compile`].
///
/// Any mappings of `map` extending outside the range of the format 4 subtable are ignored.
///
/// Note that this function does not automatically append a sentinel segment at the end of the segments vector.
///
/// See [`compile_segments`] for usage examples.
///
/// [`compile`]: compile
pub fn encoding_segments(map: &CharacterMap, ctx: &Context) -> Vec<Segment> {
    let mappings = map.iter().take_while(|(&code, _)| code < SENTINEL_SCALER);
    let mut segments: Vec<Segment> = Vec::new();
    let mut spans = mappings.spans_by_key(|(&code, _)| code as u32, |a, b| a + 1 == b);

    while let Some(span) = spans.next() {
        for segment in encoding_segments_continuous(map, span, ctx) {
            segments.push(segment);
        }
    }

    segments
}

/// Returns a segmentation of the given mappings where `mappings` is required to have continuous character codes.
///
/// The `map` parameter is used to format debugging messages.
/// `mappings` must be a slice of `map`.
pub fn encoding_segments_continuous<'a, I>(
    map: &CharacterMap,
    mappings: I,
    ctx: &Context,
) -> impl Iterator<Item = Segment>
where
    I: IntoIterator<Item = (&'a char, &'a u32)>,
{
    log::trace!("encode segments with continuous character codes ...");

    let mappings = mappings
        .into_iter()
        .take_while(|(&code, _)| code < SENTINEL_SCALER);

    // Stage 1
    //
    // Merge mappings with continuous glyph ids.
    //
    // from: 1 | 2 | 3 | 5 | 7 | 9 | 10 | 12 | 14 | 15 | 17 | 18 | 19 [104]
    //   to: 1,2,3 | 5 | 7 | 9,10 | 12 | 14,15 | 17,18,19 [56]

    log::trace!("form initial segments using glyph id discontinuities ...");

    let mut segments = Vec::<Option<Segment>>::new();
    let mut spans = mappings.spans_by_key(|(_, &x)| x, |a, b| a + 1 == b);

    while let Some(mut span) = spans.next() {
        let start = *span.next().unwrap().0;
        let end = span.last().map_or(start, |(&x, _)| x);
        let segment = Segment {
            start,
            end,
            strategy: SegmentEncodingStrategy::Shift,
        };
        segments.push(Some(segment));
    }

    log::trace!("segments: {}", debug_segments_description(map, &segments));

    // Stage 2
    //
    // Merge single mappings to mixed segments.
    //
    // from: 1,2,3 | 5 | 7 | 9,10 | 12 | 14,15 | 17,18,19 [56]
    //   to: 1,2,3 | 5,7 | 9,10 | 12 | 14,15 | 17,18,19 [52]

    log::trace!("merge consecutive single-mapping segments ...");

    // start at index `1` because `segment` in the loop is the last item in `leading`
    // (therefore accessed at `i - 1`, not `i`)
    let mut segments_iter = 1..segments.len();

    while let Some(i) = segments_iter.next() {
        let (leading, trailing) = segments.split_at_mut(i);
        // 1. stating loop at `i = 1` => `last_mut` is never `None`
        // 2. stage 1 does not merge => all segments are non-`None`
        let segment = leading.last_mut().unwrap().as_mut().unwrap();

        if segment.len() > 1 {
            continue;
        }

        'extend_mix: for other in trailing.iter_mut() {
            if let Some(other_segment) = other {
                if other_segment.len() == 1 {
                    segment.strategy = SegmentEncodingStrategy::Mix;
                    segment.end = other_segment.end;
                    *other = None;
                    segments_iter.next();
                } else {
                    break 'extend_mix;
                }
            }
        }
    }

    log::trace!("segments: {}", debug_segments_description(map, &segments));

    if !ctx.cmap_format4_advanced_segment_merging {
        return segments.into_iter().flatten();
    }

    // Stage 3
    //
    // Merge segments
    //
    // from: 1,2,3 | 5,7 | 9,10 | 12 | 14,15 | 17,18,19 [52]
    //   to: 1,2,3 | 5,7 | 9,10,12,14,15,17,18,19 [44]
    //   to: 1,2,3,5,7 | 9,10,12,14,15,17,18,19 [42]
    //   to: 1,2,3,5,7,9,10,12,14,15,17,18,19 [34]

    loop {
        log::trace!("merge segments if merged size is smaller ...");

        // Select indices to evaluate

        let mut indices_all;
        let mut indices_shortest;
        let shortest_len;

        let evaluate_indices: &mut dyn Iterator<Item = usize> =
            if ctx.cmap_format4_evaluate_all_segments {
                // evaluating all indices can result in smaller code size
                indices_all = 0..segments.len();
                &mut indices_all
            } else {
                // evaluating the shortest segments only is a good approximation
                let shortest_len_option = segments
                    .iter()
                    .flatten()
                    .min_by_key(|x| x.len())
                    .map(Segment::len);

                if shortest_len_option.is_none() {
                    break;
                }

                shortest_len = shortest_len_option.unwrap();
                indices_shortest = segments.iter().enumerate().filter_map(|(index, segment)| {
                    if let Some(segment) = segment {
                        if segment.len() == shortest_len {
                            return Some(index);
                        }
                    }
                    None
                });
                &mut indices_shortest
            };

        // Evaluate merging from `evaluate_indices`

        struct Evaluation {
            savings: usize,
            base_index: usize,
            upper_merge_index: usize,
            lower_merge_index: usize,
        }
        let mut best_match: Option<Evaluation> = None;

        for base_index in evaluate_indices {
            log::trace!("evaluating merge ranges from index {:?}", base_index);

            let (lower, segment, upper) = split_pivot(&segments, base_index);

            if segment.is_none() {
                continue;
            }

            let segment = segment.as_ref().unwrap();

            let (upper_merge_index, upper_savings) =
                evaluate_merging_segments(segment, base_index, upper.iter(), |offset| {
                    base_index + 1 + offset
                });
            let (lower_merge_index, lower_savings) =
                evaluate_merging_segments(segment, base_index, lower.iter().rev(), |offset| {
                    base_index - 1 - offset
                });
            let savings = upper_savings + lower_savings;

            let is_better_savings = best_match.as_ref().map_or(true, |x| savings > x.savings);
            let did_expand = upper_merge_index != base_index || lower_merge_index != base_index;
            let updating_merge = is_better_savings && did_expand;

            log::trace!(
                "found max savings merge range {}--{} with savings: {} + {} = {} => updating: {}",
                lower_merge_index,
                upper_merge_index,
                lower_savings,
                upper_savings,
                savings,
                updating_merge
            );

            if updating_merge {
                best_match = Some(Evaluation {
                    savings,
                    base_index,
                    upper_merge_index,
                    lower_merge_index,
                });
            }
        }

        // Check whether an expanding merge exists

        if best_match.is_none() {
            break;
        }

        let best_match = best_match.unwrap();
        let did_expand = best_match.upper_merge_index > best_match.base_index
            || best_match.lower_merge_index < best_match.base_index;

        if did_expand {
            log::trace!(
                "merge range {}--{} into {} with savings of {}",
                best_match.lower_merge_index,
                best_match.upper_merge_index,
                best_match.base_index,
                best_match.savings
            );

            let (lower, segment, upper) = split_pivot_mut(&mut segments, best_match.base_index);
            let segment = segment.as_mut().unwrap();

            segment.strategy = SegmentEncodingStrategy::Mix;

            for other in lower
                .iter_mut()
                .rev()
                .take(best_match.base_index - best_match.lower_merge_index)
            {
                if let Some(other_segment) = other {
                    segment.start = other_segment.start;
                    *other = None;
                }
            }

            for other in upper
                .iter_mut()
                .take(best_match.upper_merge_index - best_match.base_index)
            {
                if let Some(other_segment) = other {
                    segment.end = other_segment.end;
                    *other = None;
                }
            }

            log::trace!("segments: {}", debug_segments_description(map, &segments));
        } else {
            break;
        }
    }

    log::trace!(
        "final segments: {}",
        debug_segments_description(map, &segments)
    );

    segments.into_iter().flatten()
}

/// Searches for a merge index for which a merge results in a better segmentation.
///
/// This function evaluates all potential merges from a base segment to a merge candidate segment.
/// A merge combines all segments from the base segment at `base_index` to the segment at the merge index.
/// A merge is saving size if the size of the resulting merged segment is smaller than or equal to the combined size of the separate segments.
///
/// The index for the merge with the maximum savings is returned together with the maximum savings in bytes.
/// The merge index furthest from the base is selected if multiple merges have the same maximum savings.
///
/// A better segmentation is found if the returned `merge_index` differs from the given `base_index`.
/// Note that a better segmentation may have `max_savings` of 0.
/// This is because a merge that does not change the size – while not a better segmentation in and of itself – allows for more effective merging in subsequent applications of this function.
///
/// `merge_candidates` iterates from the base, excluding the base segment.
/// `offset_index` returns the index of a segment for a given offset from the base index.
/// This transformation is needed since `evaluate_merging_segments` can not know whether `merge_candidates` is ordered in increasing or decreasing order and only knows the offset from the base as provided by the iteration.
/// The initial offset is always `0` and increases with each invocation of `offset_index`.
fn evaluate_merging_segments<'a, I>(
    base: &Segment,
    base_index: usize,
    merge_candidates: I,
    offset_index: impl Fn(usize) -> usize,
) -> (usize, usize)
where
    I: IntoIterator<Item = &'a Option<Segment>>,
{
    let mut mappings_count = base.len();
    let mut current_size = base.size();
    let mut max_savings: usize = 0;
    let mut merge_index = base_index;

    for (offset, other) in merge_candidates
        .into_iter()
        .enumerate()
        .filter_map(|(i, x)| if let Some(x) = x { Some((i, x)) } else { None })
    {
        mappings_count += other.len();
        current_size += other.size();

        let mix_size = Segment::size_of_mix_segment(mappings_count);
        let savings = current_size as isize - mix_size as isize;

        if savings > max_savings as isize {
            merge_index = offset_index(offset);
            max_savings = savings as usize;
        }
    }

    (merge_index, max_savings)
}

/// A segment is a range of continuous character codes with an encoding strategy.
///
/// Segments are used when encoding a character map that maps character codes to glyph ids.
/// `start` and `end` define the range of character codes within the map.
/// The `strategy` of a segment describes which encoding strategy should be used to encode the segment.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Segment {
    /// The start of the continuous character code range.
    pub start: char,
    /// The end of the continuous character code range.
    pub end: char,
    /// The strategy used to encode the segment.
    pub strategy: SegmentEncodingStrategy,
}

impl Segment {
    /// Returns a sentinel segment which is used to end a list of segments.
    ///
    /// The segment starts and ends at [`SENTINEL_SCALER`].
    ///
    /// Quoting the [OpenType specification][spec]:
    ///
    /// > For the search to terminate, the final start code and endCode values must be 0xFFFF. This segment need not contain any valid mappings. (It can just map the single character code 0xFFFF to missingGlyph). However, the segment must be present.
    ///
    /// [spec]: https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values
    pub fn sentinel() -> Segment {
        Segment {
            start: SENTINEL_SCALER,
            end: SENTINEL_SCALER,
            strategy: SegmentEncodingStrategy::Shift,
        }
    }

    /// The size in bytes of a mix segment containing `mappings_count` mappings.
    fn size_of_mix_segment(mappings_count: usize) -> usize {
        SEGMENT_CORE_SIZE + (mappings_count * GLYPH_ID_ARRAY_FIELD_SIZE)
    }

    /// The number of mappings managed by the segment.
    pub fn len(&self) -> usize {
        ((self.end as u32) - (self.start as u32)) as usize + 1
    }

    /// The size in bytes required to represent the segment.
    ///
    /// This includes the additional size which may be required for `glyphIdArray`.
    pub fn size(&self) -> usize {
        match self.strategy {
            SegmentEncodingStrategy::Shift => SEGMENT_CORE_SIZE,
            SegmentEncodingStrategy::Mix => Segment::size_of_mix_segment(self.len()),
        }
    }
}

impl std::fmt::Debug for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.len() == 1 {
            f.write_str(&format!("<{:?}>", self.start))
        } else {
            f.write_str(&match self.strategy {
                SegmentEncodingStrategy::Shift => {
                    format!("<{:?}-{:?} ({})>", self.start, self.end, self.len())
                }
                SegmentEncodingStrategy::Mix => {
                    format!("<{:?}#{:?} ({})>", self.start, self.end, self.len())
                }
            })
        }
    }
}

/// The strategy used to encode a segment.
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub enum SegmentEncodingStrategy {
    /// The *shift* strategy shifts the codepoints of all characters by a shared offset.
    ///
    /// This strategy requires all mappings to have the same offset from character code to glyph id.
    /// Segments that can be encoded as such are represented more efficiently using the shift strategy as compared to the *mix* strategy.
    /// However, merging short shift segments into a single mix segment or merging a shift segment with a mix segment may result in an overall more efficient representation.
    ///
    /// Note that this strategy necessitates that the glyph ids of the segment are continuous.
    /// This follows from the same-offset requirement together with the fact that all mappings in any segment must have continuous character codes.
    Shift,
    /// The *mix* strategy assigns character codes to glyph ids.
    ///
    /// This strategy uses additional storage, `glyphIdArray`, to map each character code to its glyph id using `idRangeOffset`.
    ///
    /// A segment encoded using this strategy is represented less efficiently than a *shift* segment.
    /// But, since a mix segment can also represent non-continuous ranges of glyph ids, a single mix segment can replace multiple shift segments.
    Mix,
}

impl std::fmt::Debug for SegmentEncodingStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shift => f.write_str("O"),
            Self::Mix => f.write_str("X"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ctx::Optimization;
    use crate::data::CharacterMap;

    fn make_character_map(init_key: char, gids: &[u32]) -> CharacterMap {
        let mut map = CharacterMap::new();
        let mut key = init_key;

        for gid in gids {
            map.insert(key, *gid);
            key = std::char::from_u32((key as u32) + 1).unwrap();
        }

        map
    }

    fn segments_slices_size(slices: &Vec<(SegmentEncodingStrategy, Vec<u32>)>) -> usize {
        slices
            .iter()
            .map(|(strategy, segments)| match strategy {
                SegmentEncodingStrategy::Shift => SEGMENT_CORE_SIZE,
                SegmentEncodingStrategy::Mix => Segment::size_of_mix_segment(segments.len()),
            })
            .sum()
    }

    fn encoding_segments_slices(gids: &[u32]) -> Vec<(SegmentEncodingStrategy, Vec<u32>)> {
        let ctx = Context::new(Optimization::O3);
        let map = make_character_map('A', &gids);
        let segments = encoding_segments_continuous(&map, map.iter(), &ctx);
        segments
            .map(|segment| {
                let start_gid = segment.start;
                let end_gid = segment.end;
                let gids: Vec<u32> = map
                    .range(start_gid..=end_gid)
                    .map(|(_, gid)| *gid)
                    .collect();
                (segment.strategy, gids)
            })
            .collect()
    }

    macro_rules! segments_slices {
      { $($strategy:tt => [$($gid:expr),+]),+ $(,)* } => {
          {
              let mut slices: Vec<(SegmentEncodingStrategy, Vec<u32>)> = Vec::new();
              $(
                  let slice = {
                      let mut slice: Vec<u32> = Vec::new();
                      $(
                          slice.push($gid);
                      )+
                      slice
                  };
                  slices.push((SegmentEncodingStrategy::$strategy, slice));
              )+
              slices
          }
      };
  }

    macro_rules! assert_eq_size {
        ($left:expr, $right:expr) => {
            let left = $left;
            let right = $right;
            let left_size = segments_slices_size(left.as_ref());
            let right_size = segments_slices_size(right.as_ref());

            if left_size != right_size {
                eprintln!("left = {:?}", left);
                eprintln!("right = {:?}", right);
                assert_eq!(left_size, right_size);
            }
        };
    }

    #[test]
    fn test_encoding_segments_continuous_simple_ord() {
        assert_eq_size!(
            encoding_segments_slices(&vec![1, 2, 3, 4, 5]),
            segments_slices! {
                Shift => [1, 2, 3, 4, 5]
            }
        );
    }

    #[test]
    fn test_encoding_segments_continuous_single_ord() {
        assert_eq_size!(
            encoding_segments_slices(&vec![1]),
            segments_slices! {
                Shift => [1]
            }
        );
    }

    #[test]
    fn test_encoding_segments_continuous_multiple_ord() {
        assert_eq_size!(
            encoding_segments_slices(&vec![1, 2, 3, 10, 11, 12, 20, 21, 22]),
            segments_slices! {
                Shift => [1, 2, 3],
                Shift => [10, 11, 12],
                Shift => [20, 21, 22],
            }
        );
    }

    #[test]
    fn test_encoding_segments_continuous_single_mix() {
        assert_eq_size!(
            encoding_segments_slices(&vec![3, 2, 1]),
            segments_slices! {
                Mix => [3, 2, 1],
            }
        );
    }

    #[test]
    fn test_encoding_segments_continuous_isolated_mix() {
        assert_eq_size!(
            encoding_segments_slices(&vec![1, 2, 3, 4, 5, 11, 10, 40, 41, 42, 43, 44]),
            segments_slices! {
                Shift => [1, 2, 3, 4, 5],
                Mix => [11, 10],
                Shift => [40, 41, 42, 43, 44],
            }
        );
    }

    #[test]
    fn test_encoding_segments_continuous_notorious() {
        assert_eq_size!(
            encoding_segments_slices(&vec![
                2713, 2717, 2712, 2716, 2714, 2718, 2715, 2719, 2476, 2477, 2478, 2479, 2480, 2486,
                2487, 2488, 2489, 2490, 2491, 2492, 2493, 2494, 2495, 2334, 2335, 2336, 2337, 2665,
                2663, 2059, 2061, 2060, 2339, 2340, 2310, 2311, 2308, 2309, 896, 849, 1664, 1637,
                1983, 1975, 2018, 2020, 2019, 2021, 766, 1594, 418, 285, 417, 286, 419, 287, 420,
                288, 421, 289, 422, 290, 541, 510, 1100, 1073, 1102, 1075, 1101, 1074, 1187, 1153,
                1181, 1143, 1405, 1338, 1408, 1345, 1420, 1360, 1466, 1437, 1470, 1446, 1469, 1445,
                1493, 1485, 1494, 1486, 1547, 1516, 1548, 1517, 1753, 1772, 1754, 1774, 1958, 1944,
                1477, 1443, 1478, 1444, 1755, 1775, 1985, 1977, 1053, 1035, 2036, 2037, 2038, 562,
                1155, 1203, 1243, 1530, 1560, 1632, 1592, 1409, 1346, 1551, 1520, 827, 828, 809,
                1188, 1154, 1549, 1518, 1550, 1519, 1665, 1638, 2194, 2070, 2276, 2063, 2065, 902,
                1151
            ]),
            segments_slices! {
                Mix => [2713, 2717, 2712, 2716, 2714, 2718, 2715, 2719],
                Shift => [2476, 2477, 2478, 2479, 2480],
                Shift => [2486, 2487, 2488, 2489, 2490, 2491, 2492, 2493, 2494, 2495],
                Mix => [2334, 2335, 2336, 2337, 2665, 2663, 2059, 2061, 2060, 2339, 2340, 2310, 2311, 2308, 2309, 896, 849, 1664, 1637, 1983, 1975, 2018, 2020, 2019, 2021, 766, 1594, 418, 285, 417, 286, 419, 287, 420, 288, 421, 289, 422, 290, 541, 510, 1100, 1073, 1102, 1075, 1101, 1074, 1187, 1153, 1181, 1143, 1405, 1338, 1408, 1345, 1420, 1360, 1466, 1437, 1470, 1446, 1469, 1445, 1493, 1485, 1494, 1486, 1547, 1516, 1548, 1517, 1753, 1772, 1754, 1774, 1958, 1944, 1477, 1443, 1478, 1444, 1755, 1775, 1985, 1977, 1053, 1035, 2036, 2037, 2038, 562, 1155, 1203, 1243, 1530, 1560, 1632, 1592, 1409, 1346, 1551, 1520, 827, 828, 809, 1188, 1154, 1549, 1518, 1550, 1519, 1665, 1638, 2194, 2070, 2276, 2063, 2065, 902, 1151],
            }
        );
    }

    fn encoding_segments_slices_brute_force(
        gids: &[u32],
    ) -> Option<Vec<(SegmentEncodingStrategy, Vec<u32>)>> {
        let len = gids.len();
        let count = 2usize.pow((len - 1) as u32);

        let mut slices: Vec<Vec<(SegmentEncodingStrategy, Vec<u32>)>> = Vec::with_capacity(count);

        for i in 0..count {
            let segments_count = i.count_ones() as usize + 1;
            let mut setting: Vec<Vec<u32>> = Vec::with_capacity(segments_count);
            let mut strategies: Vec<SegmentEncodingStrategy> = Vec::with_capacity(segments_count);
            let mut previous_gid = gids[0];
            setting.push(vec![previous_gid]);
            strategies.push(SegmentEncodingStrategy::Shift);

            // Iterate over the binary digits of `i`.
            // The first digit is always `1` and has already been handled by the `setting.push` invocation above.
            for j in 1..len {
                // Split segment if digit is `1`; push to current segment if digit is `0`.
                let gid = gids[j];

                if (1 & (i >> j - 1)) == 1 {
                    setting.push(vec![gid]);
                    strategies.push(SegmentEncodingStrategy::Shift);
                    previous_gid = 0;
                } else {
                    let offset = setting.len() - 1;
                    setting[offset].push(gid);
                }

                if previous_gid != 0 && previous_gid + 1 != gid {
                    let offset = strategies.len() - 1;
                    strategies[offset] = SegmentEncodingStrategy::Mix;
                }

                previous_gid = gid;
            }

            slices.push(strategies.into_iter().zip(setting).collect());
        }

        slices.into_iter().min_by_key(segments_slices_size)
    }

    #[test]
    fn test_encoding_segments_brute_force_notorious() {
        let tests = vec![
            vec![2, 1, 3, 4, 5],
            vec![1, 2, 3, 4, 5, 10, 20, 21],
            vec![1, 2, 10, 20, 21, 22, 23, 24],
            vec![1, 10, 11, 12, 13, 21, 20],
            vec![1, 2, 3, 4, 5, 7, 6],
            vec![1, 2, 3, 4, 5, 7, 8, 6],
            vec![2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 16, 17, 19, 20],
            vec![3, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 18, 19, 21],
            vec![2, 3, 5, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19],
            vec![2, 3, 5, 6, 7, 8, 9, 11, 12, 14, 15, 16, 18, 19, 20],
            vec![1, 2, 3, 4, 5, 11, 10, 40, 41, 42, 43, 44],
            vec![2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 19, 20],
        ];

        for test in tests {
            eprintln!("{:?} ...", test);

            let optimal_slices = encoding_segments_slices_brute_force(&test).unwrap();
            let optimal_size = segments_slices_size(&optimal_slices);
            let slices = encoding_segments_slices(&test);
            let size = segments_slices_size(&slices);

            if size > optimal_size {
                eprintln!("optimal:      {:?}", optimal_slices);
                eprintln!("segmentation: {:?}", slices);
                assert_eq!(optimal_size, size);
            }
        }
    }

    #[test]
    fn test_encoding_segments_brute_force_all_permutations() {
        for i in 1u32..=7u32 {
            let mut range: Vec<u32> = (1u32..=i).collect();

            eprintln!("range: {:?}", range);

            let heap = permutohedron::Heap::new(&mut range);

            for test in heap {
                let optimal_slices = encoding_segments_slices_brute_force(&test).unwrap();
                let optimal_size = segments_slices_size(&optimal_slices);
                let slices = encoding_segments_slices(&test);
                let size = segments_slices_size(&slices);

                if size > optimal_size {
                    eprintln!("optimal:      {:?}", optimal_slices);
                    eprintln!("segmentation: {:?}", slices);
                    assert_eq!(optimal_size, size);
                }
            }
        }
    }

    #[allow(unused)]
    fn test_encoding_segments_brute_force_random() {
        use rand::prelude::*;
        let mut rng = rand::thread_rng();

        let tests_size = 22;
        let test_count = 1000;

        for _ in 0..test_count {
            let mut test: Vec<u32> = Vec::new();
            let mut value: u32 = 1;

            for _ in 0..tests_size {
                value += if rng.gen::<f64>() < 0.8 { 1 } else { 2 };
                test.push(value);
            }

            eprintln!("{:?} ...", test);

            let optimal_slices = encoding_segments_slices_brute_force(&test).unwrap();
            let optimal_size = segments_slices_size(&optimal_slices);
            let slices = encoding_segments_slices(&test);
            let size = segments_slices_size(&slices);

            if size > optimal_size {
                eprintln!("optimal:      {:?}", optimal_slices);
                eprintln!("segmentation: {:?}", slices);
                assert_eq!(optimal_size, size);
            }
        }
    }
}
