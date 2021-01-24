//! # Format 12: Segmented coverage
//!
//! Implementation of the `cmap` format 12 subtable.
//!
//! *Specification:*
//! [OpenType](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-12-segmented-coverage),
//! [TrueType](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cmap.html).
//!
//! The entry to this module is the [`compile`] function.
//! It returns the subtable bytes for a given character map.
//!
//! ```
//! # use informa::data::CharacterMap;
//! # use informa::sfnt::tables::cmap::format12::compile;
//! let mut map = CharacterMap::new();
//! map.insert('A', 1);
//! map.insert('B', 2);
//! map.insert('C', 3);
//! let subtable = compile(&map);
//! ```
//!
//! [`compile_groups`] allows manual control over the groups used.
//! [`form_groups`] returns groups for a character map.
//!
//! ```
//! # use informa::data::CharacterMap;
//! # use informa::sfnt::tables::cmap::format12::form_groups;
//! # use informa::sfnt::tables::cmap::format12::compile_groups;
//! let mut map = CharacterMap::new();
//! # map.insert('A', 1);
//! # map.insert('B', 2);
//! # map.insert('C', 3);
//! // ... insert entries into character map ...
//! let groups = form_groups(&map);
//! let subtable = compile_groups(&groups);
//! ```

use crate::data::CharacterMap;
use bytes::{BufMut, Bytes, BytesMut};
use spans::Spans;
use std::mem::size_of;

/// The size in bytes of the `format` field.
const FORMAT_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `reserved` field.
const RESERVED_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `length` field.
const LENGTH_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of the `language` field.
const LANGUAGE_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of the `numGroups` field.
const NUM_GROUPS_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of a `startCharCode` field.
const START_CHAR_CODE_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of an `endCharCode` field.
const END_CHAR_CODE_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of a `startGlyphID` field.
const START_GLYPH_ID_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of the subtable header.
const CONSTANT_SIZE: usize = FORMAT_FIELD_SIZE
    + RESERVED_FIELD_SIZE
    + LENGTH_FIELD_SIZE
    + LANGUAGE_FIELD_SIZE
    + NUM_GROUPS_FIELD_SIZE;
/// The size in bytes of a sequential map group.
const GROUP_SIZE: usize =
    START_CHAR_CODE_FIELD_SIZE + END_CHAR_CODE_FIELD_SIZE + START_GLYPH_ID_FIELD_SIZE;

/// Returns a `cmap` format 12 subtable representing the given character map.
///
/// # Example
///
/// ```
/// # use informa::data::CharacterMap;
/// # use informa::sfnt::tables::cmap::format12::compile;
/// let mut map = CharacterMap::new();
/// map.insert('A', 1);
/// map.insert('B', 2);
/// map.insert('C', 3);
/// let subtable = compile(&map);
/// ```
pub fn compile(map: &CharacterMap) -> Bytes {
    let groups = form_groups(map);
    compile_groups(&groups)
}

/// Returns a `cmap` format 12 subtable representing the given groups.
///
/// The groups need to be in order of increasing `start_char_code`.
///
/// Use [`form_groups`] to generate a vector of groups from a character map.
///
/// ```
/// # use informa::data::CharacterMap;
/// # use informa::sfnt::tables::cmap::format12::form_groups;
/// # use informa::sfnt::tables::cmap::format12::compile_groups;
/// let mut map = CharacterMap::new();
/// map.insert('A', 1);
/// map.insert('B', 2);
/// map.insert('C', 3);
/// let groups = form_groups(&map);
/// let subtable = compile_groups(&groups);
/// ```
pub fn compile_groups(groups: &[SequentialMapGroup]) -> Bytes {
    let num_groups = groups.len();
    let length = CONSTANT_SIZE + (num_groups * GROUP_SIZE);
    let mut buf = BytesMut::with_capacity(length);

    let format = 12;
    buf.put_u16(format);

    let reserved = 0;
    buf.put_u16(reserved);

    buf.put_u32(length as u32);

    let language = 0;
    buf.put_u32(language);

    buf.put_u32(num_groups as u32);

    for group in groups {
        buf.put_u32(group.start_char_code);
        buf.put_u32(group.end_char_code);
        buf.put_u32(group.start_glyph_id);
    }

    assert_eq!(length, buf.len());

    buf.freeze()
}

/// Returns a vector of groups representing the given character map.
///
/// The groups are returned in order of increasing `start_char_code`.
///
/// See [`compile_groups`] for usage examples.
pub fn form_groups(map: &CharacterMap) -> Vec<SequentialMapGroup> {
    let mut groups: Vec<SequentialMapGroup> = Vec::new();
    let mut spans = map.iter().spans_by_key(
        |(&code, &gid)| (code as u32, gid),
        |(code_a, gid_a), (code_b, gid_b)| code_a + 1 == code_b && gid_a + 1 == gid_b,
    );

    while let Some(mut span) = spans.next() {
        let (&start_char, &start_glyph_id) = span.next().unwrap();
        let end_char = span.last().map_or(start_char, |(&x, _)| x);
        let group = SequentialMapGroup {
            start_char_code: start_char as u32,
            end_char_code: end_char as u32,
            start_glyph_id,
        };
        groups.push(group);
    }

    groups
}

/// A continuous range of character codes where all character codes map to glyph ids with the same offset.
///
/// It follows that the glyph ids of the group are also continuous.
/// The offset from character code to glyph id is expressed by the difference between the `start_char_code` and the `start_glyph_id`.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SequentialMapGroup {
    /// The character code of the start of the range.
    pub start_char_code: u32,
    /// The character code of the end of the range.
    pub end_char_code: u32,
    /// The glyph id corresponding to `start_char_code`.
    pub start_glyph_id: u32,
}
