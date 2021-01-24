//! Types used throughout an SFNT file.

use crate::util::byte::ByteExt;

/// Interpretation of four bytes as a four-letter tag.
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct Tag(pub [u8; 4]);

impl Tag {
    /// Creates a tag by interpreting a `u32` as 4 bytes with a big-endian layout.
    ///
    /// Returns `None` if any of the bytes are not in the range from `0x20` to `0x7E` (both inclusive). This value restriction matches the [OpenType specification for tags](https://docs.microsoft.com/en-us/typography/opentype/spec/otff#data-types).
    #[inline]
    pub fn from_u32(value: u32) -> Option<Tag> {
        let bytes = value.to_be_bytes();

        if bytes.iter().any(|&x| x < 0x20 || x > 0x7E) {
            None
        } else {
            Some(Tag(bytes))
        }
    }
}

impl From<Tag> for u32 {
    fn from(tag: Tag) -> u32 {
        u32::from_be_bytes(tag.into())
    }
}

impl From<Tag> for [u8; 4] {
    fn from(tag: Tag) -> [u8; 4] {
        let Tag(bytes) = tag;
        bytes
    }
}

impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Tag(bytes) = self;
        write!(
            f,
            "'{}{}{}{}'",
            bytes[0] as char, bytes[1] as char, bytes[2] as char, bytes[3] as char
        )
    }
}

impl std::fmt::Debug for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Tag(bytes) = self;
        write!(
            f,
            "{}{}{}{}",
            bytes[0].picture(),
            bytes[1].picture(),
            bytes[2].picture(),
            bytes[3].picture(),
        )
    }
}
