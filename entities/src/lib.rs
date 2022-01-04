#[macro_use]
extern crate cfg_if;

#[macro_use]
extern crate serde_with;

mod as_enum;
mod dependent_fields;
mod error;

pub use as_enum::*;
pub use dependent_fields::*;
pub use error::*;
pub use paste::paste as entities_paste;
