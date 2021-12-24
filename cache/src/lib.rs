pub use owning_ref as hex_arch_cache_owning_ref;
pub use proc_macros::*;

use uuid::Uuid;

#[derive(Clone, Debug)]
pub enum CacheError {
    CouldNotConfirmCacheValidity,
    CouldNotUpdateCacheValidity,
    CouldNotLoadResources,
}

pub fn default_cache_uuid() -> String {
    Uuid::new_v4().to_hyphenated().to_string()
}
