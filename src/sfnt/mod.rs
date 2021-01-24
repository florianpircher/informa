//! Implementation of the SFNT container format.

pub mod tables;
pub mod types;

use crate::ctx::Context;
use crate::data::Source;
use bytes::{BufMut, Bytes, BytesMut};
use std::convert::TryInto;
use std::mem::size_of;
use types::Tag;

/// The size in bytes of the `sfntVersion` field.
const SFNT_VERSION_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of the `numTables` field.
const NUM_TABLES_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `searchRange` field.
const SEARCH_RANGE_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `entrySelector` field.
const ENTRY_SELECTOR_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `rangeShift` field.
const RANGE_SHIFT_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of a `tableTag` field.
const TABLE_TAG_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of a `checksum` field.
const CHECKSUM_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of an `offset` field.
const OFFSET_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of a `length` field.
const LENGTH_FIELD_SIZE: usize = size_of::<u32>();
/// The size in bytes of the table directory preamble.
const PREAMBLE_SIZE: usize = SFNT_VERSION_FIELD_SIZE
    + NUM_TABLES_FIELD_SIZE
    + SEARCH_RANGE_FIELD_SIZE
    + ENTRY_SELECTOR_FIELD_SIZE
    + RANGE_SHIFT_FIELD_SIZE;
/// The size in bytes of a `TableRecord`.
const TABLE_RECORD_SIZE: usize =
    TABLE_TAG_FIELD_SIZE + CHECKSUM_FIELD_SIZE + OFFSET_FIELD_SIZE + LENGTH_FIELD_SIZE;

/// Returns the checksum of a table.
fn table_checksum(table: &[u8]) -> u32 {
    // NOTE: use `array_chunks` or `as_chunks` once stable and remove `try_into` below
    let iter = table.chunks_exact(4);
    let remainder = iter.remainder();

    let sum = iter.fold(0u32, |a, x| {
        // slice `x` is convertible to `&[u8, 4]` because of `chunks_exact(4)`
        a.wrapping_add(u32::from_be_bytes(x.try_into().unwrap()))
    });

    // add zero-padding to remainder
    let mut rest = [0; 4];

    for (offset, byte) in remainder.iter().enumerate() {
        rest[offset] = *byte;
    }

    sum + u32::from_be_bytes(rest)
}

/// Compiles an SFNT font.
///
/// `sfnt_version` is written to the first four bytes of the font.
/// The tables are written in the given order to the font.
///
/// # Example
///
/// ```
/// # use informa::ctx::Context;
/// # use informa::data::Source;
/// # use informa::sfnt::types::Tag;
/// # use informa::sfnt::compile;
/// use informa::sfnt::tables;
///
/// let ctx = Context::default();
/// let source = Source::default();
/// let sfnt_version = Tag(*b"OTTO").as_u32();
/// let tables = vec![
///     (Tag(*b"cmap"), tables::cmap::compile(&source.character_map, ctx)),
///     // ... tables ...
/// ];
///
/// let font = compile(sfnt_version, tables);
/// ```
pub fn compile(sfnt_version: u32, tables: Vec<(Tag, Bytes)>) -> Bytes {
    log::info!(
        "SFNT version: 0x{:X}{}",
        sfnt_version,
        Tag::from_u32(sfnt_version).map_or(String::new(), |x| format!(" ({})", x))
    );
    log::info!("Write Table Directory Preamble");

    let length = PREAMBLE_SIZE
        + (tables.len() * TABLE_RECORD_SIZE)
        + tables.iter().map(|(_, x)| x.len()).sum::<usize>();
    let mut buf = BytesMut::with_capacity(length);

    buf.put_u32(sfnt_version);

    let num_tables: u16 = tables.len() as u16;
    buf.put_u16(num_tables);

    let search_range: u16 = 16 * 2u16.pow((num_tables as f64).log2().floor() as u32);
    buf.put_u16(search_range);

    let entry_selector: u16 = (num_tables as f64).log2().floor() as u16;
    buf.put_u16(entry_selector);

    let range_shift: u16 = num_tables * 16 - search_range;
    buf.put_u16(range_shift);

    log::info!("Write Table Records");

    let mut offset: u32 = (PREAMBLE_SIZE + (tables.len() * TABLE_RECORD_SIZE)) as u32;

    for (tag, table) in &tables {
        let tag = *tag;
        buf.put_u32(tag.into());

        let checksum = table_checksum(table);
        buf.put_u32(checksum);

        buf.put_u32(offset);

        let len = table.len() as u32;
        buf.put_u32(len);

        let padding = (4 - (len % 4)) % 4;
        offset += len + padding;
    }

    log::info!("Write Tables");

    for (tag, table) in tables {
        log::info!("writing table '{}' ...", tag);

        let len = table.len();

        buf.put(table);

        let padding = (4 - (len % 4)) % 4;

        for _ in 0..padding {
            buf.put_u8(0);
        }
    }

    assert_eq!(length, buf.len());

    buf.freeze()
}

/// Compiles a CFF flavoured OpenType font.
pub fn compile_otf(source: &Source, ctx: &Context) -> Bytes {
    let sfnt_version: u32 = Tag(*b"OTTO").into();
    let tables = vec![(
        Tag(*b"cmap"),
        tables::cmap::compile(&source.character_map, ctx),
    )];

    compile(sfnt_version, tables)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_table_checksum() {
        assert_eq!(table_checksum(b"abcd"), 1633837924);
        assert_eq!(table_checksum(b"abcdxyz"), 3655064932);
    }
}
