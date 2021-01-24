//! # The Informa Font Compiler
//!
//! *Informa* is a font compiler for digital typography.
//!
//! This project is work-in-progress.
//! The current focus is on producing SFNT fonts, in particular OpenType fonts.
//!
//! The architecture of the compiler is designed to offer a high-level API for the common case and a low-level API for custom font compilation.
//! Additionally, a rich set of compilation options and customization points allows tuning the compiler without using the low-level API.
//!
//! ## Fully supported components
//!
//! - The [SFNT container format](crate::sfnt).
//! - The [`cmap` table](crate::sfnt::tables::cmap) with [format 4](crate::sfnt::tables::cmap::format4) and [format 12](crate::sfnt::tables::cmap::format12) subtables.

#![deny(missing_docs, missing_debug_implementations)]

pub mod ctx;
pub mod data;
pub mod sfnt;
mod util;
