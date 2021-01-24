//! # cmap — Character to Glyph Index Mapping Table
//!
//! Implementation of the `cmap` table.
//!
//! *Specification:*
//! [OpenType](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap),
//! [TrueType](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cmap.html).
//!
//! The `cmap` table provides mappings from character codes to glyph ids.
//! This allows typesetting systems using the font to resolve the glyphs needed to display a string of characters.
//!
//! Not all glyphs need be covered by `cmap`; some glyphs are accessible only using smart font technology or a glyph picker.
//! Conversely, some glyphs may be mapped to by multiple character codes.
//!
//! A `cmap` table consists of one or multiple subtables.
//! Subtables are differentiated by their format.
//! Formats differ in character code encoding, compression strategies, and codepoint space size.
//!
//! Subtables are accessed by encoding records.
//! A record describes the format of its associated subtable, the intended platform, and the encoding.
//! Multiple records using the same subtable format (but possibly with different platforms and encodings) need not store the same subtable multiple times and instead share the common subtable storage space.
//!
//! # Usage
//!
//! Use the [`compile`] function to convert a character map to a `cmap` table.
//! The [`Context::cmap_encoding_records`] field can be set to customize the encoding records used by [`compile`].
//!
//! # Example
//!
//! ```
//! # use informa::ctx::Context;
//! # use informa::data::CharacterMap;
//! # use informa::sfnt::tables::cmap::compile;
//! let ctx = Context::default();
//! let mut map = CharacterMap::new();
//! map.insert('A', 1);
//! map.insert('B', 2);
//! map.insert('C', 3);
//! let table = compile(&map, &ctx);
//! ```

pub mod format12;
pub mod format4;

use crate::ctx::Context;
use crate::data::CharacterMap;
use bytes::{BufMut, Bytes, BytesMut};
use itertools::Itertools;
use lazy_static::lazy_static;
use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::mem::size_of;

/// The size in bytes of the `version` field.
const VERSION_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `numTables` field.
const NUM_TABLES_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `platformID` field.
const PLATFORM_ID_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `encodingID` field.
const ENCODING_ID_FIELD_SIZE: usize = size_of::<u16>();
/// The size in bytes of the `subtableOffset` field.
const SUBTABLE_OFFSET_FIELD_SIZE: usize = size_of::<u32>();
/// The size if bytes of an encoding record.
const ENCODING_RECORD_SIZE: usize =
    PLATFORM_ID_FIELD_SIZE + ENCODING_ID_FIELD_SIZE + SUBTABLE_OFFSET_FIELD_SIZE;
/// The size in bytes of the table header.
const CONSTANT_SIZE: usize = VERSION_FIELD_SIZE + NUM_TABLES_FIELD_SIZE;
/// The largest Unicode scaler that is part of the Basic Multilingual Plane (BMP).
const MAX_BMP_SCALER: char = '\u{FFFF}';

lazy_static! {
    /// The default records used to represent BMP only character maps in order of `Ord`.
    static ref DEFAULT_BMP_RECORDS: Vec<EncodingRecord> = vec![
        EncodingRecord {
            encoding: Encoding::Unicode(UnicodeEncoding::Bmp),
            format: RecordFormat::Format4,
        },
        EncodingRecord {
            encoding: Encoding::Windows(WindowsEncoding::Bmp),
            format: RecordFormat::Format4,
        },
    ];
    /// The default records used to represent full Unicode character maps in order of `Ord`.
    static ref DEFAULT_FULL_RECORDS: Vec<EncodingRecord> = vec![
        EncodingRecord {
            encoding: Encoding::Unicode(UnicodeEncoding::Bmp),
            format: RecordFormat::Format4,
        },
        EncodingRecord {
            encoding: Encoding::Unicode(UnicodeEncoding::Full),
            format: RecordFormat::Format12,
        },
        EncodingRecord {
            encoding: Encoding::Windows(WindowsEncoding::Bmp),
            format: RecordFormat::Format4,
        },
        EncodingRecord {
            encoding: Encoding::Windows(WindowsEncoding::Full),
            format: RecordFormat::Format12,
        },
    ];
}

/// Returns a `cmap` table for the given character map.
///
/// # Example
///
/// ```
/// # use informa::ctx::Context;
/// # use informa::data::CharacterMap;
/// # use informa::sfnt::tables::cmap::compile;
/// let ctx = Context::default();
/// let mut map = CharacterMap::new();
/// map.insert('A', 1);
/// map.insert('B', 2);
/// map.insert('C', 3);
/// let table = compile(&map, &ctx);
/// ```
pub fn compile(map: &CharacterMap, ctx: &Context) -> Bytes {
    let records = ctx.cmap_encoding_records.as_ref().unwrap_or_else(|| {
        let exceeds_bmp = map.keys().last().map_or(false, |&x| x > MAX_BMP_SCALER);
        if exceeds_bmp {
            &DEFAULT_FULL_RECORDS
        } else {
            &DEFAULT_BMP_RECORDS
        }
    });

    let mut subtables: HashMap<RecordFormat, Bytes> = records
        .iter()
        .map(|x| x.format)
        .unique()
        .map(|format| {
            let subtable = match format {
                RecordFormat::Format4 => format4::compile(map, ctx),
                RecordFormat::Format12 => format12::compile(map),
            };

            (format, subtable)
        })
        .collect();

    let fixed_size = CONSTANT_SIZE + (records.len() * ENCODING_RECORD_SIZE);
    let length = fixed_size + subtables.values().map(Bytes::len).sum::<usize>();
    let mut buf = BytesMut::with_capacity(length);

    let version: u16 = 0;
    buf.put_u16(version);

    let num_tables: u16 = records.len() as u16;
    buf.put_u16(num_tables);

    let mut subtable_offsets: HashMap<RecordFormat, u32> = HashMap::new();
    let mut subtable_offset: u32 = fixed_size as u32;

    for record in records {
        let platform_id = record.encoding.platform_id();
        buf.put_u16(platform_id);

        let encoding_id = record.encoding.encoding_id();
        buf.put_u16(encoding_id);

        if let Some(&offset) = subtable_offsets.get(&record.format) {
            // reuse already registered subtable
            buf.put_u32(offset);
        } else {
            // register subtable format
            buf.put_u32(subtable_offset);
            subtable_offsets.insert(record.format, subtable_offset);
            subtable_offset += subtables[&record.format].len() as u32;
        }
    }

    for format in records.iter().map(|x| x.format).unique() {
        let subtable = subtables.get_mut(&format).unwrap();
        buf.put(subtable);
    }

    assert_eq!(length, buf.len());

    buf.freeze()
}

/// An encoding record describes a `cmap` subtable.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct EncodingRecord {
    /// The encoding used by the subtable.
    pub encoding: Encoding,
    /// The subtable format.
    pub format: RecordFormat,
}

impl cmp::PartialOrd for EncodingRecord {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.encoding.partial_cmp(&other.encoding)
    }
}

impl cmp::Ord for EncodingRecord {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.encoding.cmp(&other.encoding)
    }
}

/// An error that may occur when reading an encoding record.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ReadError {
    /// The syntax is invalid.
    InvalidSyntax,
    /// The platform id can not be parsed.
    InvalidPlatform,
    /// The encoding id can not be parsed.
    InvalidEncoding,
    /// The format id can not be parsed.
    InvalidFormat,
    /// The platform id is not supported.
    UnsupportedPlatform,
    /// The encoding id is not supported.
    UnsupportedEncoding,
    /// The format is not supported.
    UnsupportedFormat,
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self {
            Self::InvalidSyntax => "The syntax is invalid.",
            Self::InvalidPlatform => "The platform id can not be parsed.",
            Self::InvalidEncoding => "The encoding id can not be parsed.",
            Self::InvalidFormat => "The format id can not be parsed.",
            Self::UnsupportedPlatform => "The platform id is not supported by Informa.",
            Self::UnsupportedEncoding => "The encoding id is not supported by Informa.",
            Self::UnsupportedFormat => "The format is not supported by Informa.",
        };
        write!(f, "{}", message)
    }
}

impl std::error::Error for ReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl EncodingRecord {
    /// Creates an `EncodingRecord` from an encoding-record code.
    ///
    /// Codes are formatted as `<platform_id> "/" <encoding_id> "=" <format_id>`.
    /// Such a code is also written using the `Display` formatting of an `EncodingRecord`.
    ///
    /// ## Example
    ///
    /// ```
    /// # use informa::sfnt::tables::cmap::EncodingRecord;
    /// # use informa::sfnt::tables::cmap::Encoding;
    /// # use informa::sfnt::tables::cmap::UnicodeEncoding;
    /// # use informa::sfnt::tables::cmap::RecordFormat;
    /// assert_eq!(
    ///     EncodingRecord::from_code("0/3=4"),
    ///     Ok(EncodingRecord {
    ///         encoding: Encoding::Unicode(UnicodeEncoding::Bmp),
    ///         format: RecordFormat::Format4
    ///     })
    /// );
    /// ```
    pub fn from_code(code: &str) -> Result<EncodingRecord, ReadError> {
        // NOTE: use `split_once` once stable
        // currently, inputs such as "0/3/abc=4=xyz" are accepted since
        // only the first two items of each split are considered
        let (selection, format) = code
            .split("=")
            .next_tuple::<(&str, &str)>()
            .ok_or(ReadError::InvalidSyntax)?;
        let (platform, encoding) = selection
            .split("/")
            .next_tuple::<(&str, &str)>()
            .ok_or(ReadError::InvalidSyntax)?;
        let platform_id = platform
            .parse::<u16>()
            .ok()
            .ok_or(ReadError::InvalidPlatform)?;
        let encoding_id = encoding
            .parse::<u16>()
            .ok()
            .ok_or(ReadError::InvalidEncoding)?;
        let format_id = format.parse::<u16>().ok().ok_or(ReadError::InvalidFormat)?;
        let encoding = Encoding::from_ids(platform_id, encoding_id)?;
        let format = RecordFormat::from_id(format_id).ok_or(ReadError::UnsupportedFormat)?;

        Ok(EncodingRecord { encoding, format })
    }
}

impl fmt::Display for EncodingRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}={}", self.encoding, self.format)
    }
}

/// The combination of a platform id and a matching encoding id.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Encoding {
    /// The Unicode platform.
    Unicode(UnicodeEncoding),
    /// The Windows platform.
    Windows(WindowsEncoding),
}

impl Encoding {
    /// Creates an encoding from a given `platformID` and `encodingID`.
    ///
    /// # Errors
    ///
    /// This function returns an error in case the platform id is unsupported or the encoding id is unsupported.
    pub fn from_ids(platform_id: u16, encoding_id: u16) -> Result<Self, ReadError> {
        match platform_id {
            0 => {
                let encoding =
                    UnicodeEncoding::from_id(encoding_id).ok_or(ReadError::UnsupportedEncoding)?;
                Ok(Encoding::Unicode(encoding))
            }
            3 => {
                let encoding =
                    WindowsEncoding::from_id(encoding_id).ok_or(ReadError::UnsupportedEncoding)?;
                Ok(Encoding::Windows(encoding))
            }
            _ => Err(ReadError::UnsupportedPlatform)?,
        }
    }

    /// The `platformID` of the encoding.
    pub fn platform_id(&self) -> u16 {
        match self {
            Self::Unicode(_) => 0,
            Self::Windows(_) => 3,
        }
    }

    /// The `encodingID` of the encoding.
    pub fn encoding_id(&self) -> u16 {
        match self {
            Self::Unicode(encoding) => encoding.id(),
            Self::Windows(encoding) => encoding.id(),
        }
    }
}

impl fmt::Display for Encoding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.platform_id(), self.encoding_id())
    }
}

/// The supported Unicode encodings.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy)]
pub enum UnicodeEncoding {
    /// The Unicode Basic Multilingual Plane codespace.
    Bmp,
    /// The full Unicode codespace.
    Full,
}

impl UnicodeEncoding {
    /// Returns a `UnicodeEncoding` for an `encodingID`; returns `None` if the encoding id is unsupported.
    pub fn from_id(id: u16) -> Option<Self> {
        match id {
            3 => Some(Self::Bmp),
            4 => Some(Self::Full),
            _ => None,
        }
    }

    /// Returns the `encodingID` of the encoding.
    pub fn id(&self) -> u16 {
        match self {
            Self::Bmp => 3,
            Self::Full => 4,
        }
    }
}

/// The supported Windows encodings.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy)]
pub enum WindowsEncoding {
    /// The Unicode Basic Multilingual Plane codespace.
    Bmp,
    /// The full Unicode codespace.
    Full,
}

impl WindowsEncoding {
    /// Returns a `WindowsEncoding` for an `encodingID`; returns `None` if the encoding id is unsupported.
    pub fn from_id(id: u16) -> Option<Self> {
        match id {
            1 => Some(Self::Bmp),
            10 => Some(Self::Full),
            _ => None,
        }
    }

    /// Returns the `encodingID` of the encoding.
    pub fn id(&self) -> u16 {
        match self {
            Self::Bmp => 1,
            Self::Full => 10,
        }
    }
}

/// The supported `cmap` subtable formats.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum RecordFormat {
    /// The subtable format 4.
    Format4,
    /// The subtable format 12.
    Format12,
}

impl RecordFormat {
    /// Returns the `RecordFormat` for a subtable format id; returns `None` if the format id is unsupported.
    pub fn from_id(id: u16) -> Option<Self> {
        match id {
            4 => Some(Self::Format4),
            12 => Some(Self::Format12),
            _ => None,
        }
    }

    /// Returns the id of the format.
    pub fn id(&self) -> u16 {
        match self {
            Self::Format4 => 4,
            Self::Format12 => 12,
        }
    }
}

impl fmt::Display for RecordFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id())
    }
}
