/// Extens `u8` by `picture`.
pub trait ByteExt {
    /// Returns a character representing the byte.
    ///
    /// The returned character is the same as `self as char` with the exception of non-printing characters (`0x00` to `0x1F`), the space character (`0x20`), and the delete character (`0x7F`).
    ///
    /// Exceptional characters are instead represented by “Control Pictures” as defined by Unicode: <https://www.unicode.org/charts/PDF/U2400.pdf>.
    fn picture(&self) -> char;
}

impl ByteExt for u8 {
    fn picture(&self) -> char {
        match self {
            0x00 => '␀',
            0x01 => '␁',
            0x02 => '␂',
            0x03 => '␃',
            0x04 => '␄',
            0x05 => '␅',
            0x06 => '␆',
            0x07 => '␇',
            0x08 => '␈',
            0x09 => '␉',
            0x0A => '␊',
            0x0B => '␋',
            0x0C => '␌',
            0x0D => '␍',
            0x0E => '␎',
            0x0F => '␏',
            0x10 => '␐',
            0x11 => '␑',
            0x12 => '␒',
            0x13 => '␓',
            0x14 => '␔',
            0x15 => '␕',
            0x16 => '␖',
            0x17 => '␗',
            0x18 => '␘',
            0x19 => '␙',
            0x1A => '␚',
            0x1B => '␛',
            0x1C => '␜',
            0x1D => '␝',
            0x1E => '␞',
            0x1F => '␟',
            0x20 => '␠',
            0x7F => '␡',
            other => *other as char,
        }
    }
}
