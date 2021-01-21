//! The source data of a font.

use std::collections::BTreeMap;

/// The source of a font.
#[derive(Debug)]
pub struct Source {
    /// The name of the font family.
    pub family_name: String,
    /// The style name of the font.
    pub style_name: String,
    /// The full name of the font.
    pub font_name: String,
    /// The character map used by the font.
    pub character_map: CharacterMap,
}

/// Maps Unicode scalers to glyph ids.
///
/// A `BTreeMap` is used since a character map is frequently accessed in ascending order of character codes.
pub type CharacterMap = BTreeMap<char, u32>;
