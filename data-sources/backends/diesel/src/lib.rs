#[macro_use]
extern crate cfg_if;

#[cfg(feature = "audit")] pub use hex_arch_data_sources_backends_diesel_audit::*;

use diesel::{
    self,
    connection::{Connection as DieselConnection},
    result::Error as DieselError,
};
use repositories::Transactional;

pub struct TransactionWrapper<'a, Connection>(pub &'a Connection);

impl<Connection> std::clone::Clone for TransactionWrapper<'_, Connection> {
    fn clone(&self) -> Self {
        TransactionWrapper(self.0)
    }
}

impl<Connection> std::marker::Copy for TransactionWrapper<'_, Connection> {}

impl<'a, Connection> From<&'a Connection> for TransactionWrapper<'a, Connection> {
    fn from(connection: &'a Connection) -> Self {
        TransactionWrapper(connection)
    }
}

impl<Connection> std::ops::Deref for TransactionWrapper<'_, Connection> {
    type Target = Connection;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Connection: DieselConnection> Transactional for TransactionWrapper<'_, Connection> {
    type AdaptorError = DieselError;

    fn with_transaction<T, E, F>(&self, f: F) -> Result<T, E>
    where
        F: FnOnce() -> Result<T, E>,
        E: From<Self::AdaptorError>
    {
        self.0.transaction(f)
    }
}

impl<Connection: DieselConnection> Transactional for &TransactionWrapper<'_, Connection> {
    type AdaptorError = DieselError;

    fn with_transaction<T, E, F>(&self, f: F) -> Result<T, E>
    where
        F: FnOnce() -> Result<T, E>,
        E: From<Self::AdaptorError>
    {
        self.0.transaction(f)
    }
}

cfg_if! {
    if #[cfg(feature = "mysql")] {
        use diesel::mysql::MysqlConnection;
        impl<'a> Into<&'a MysqlConnection> for TransactionWrapper<'a, MysqlConnection> {
            fn into(self) -> &'a MysqlConnection {
                self.0
            }
        }
    }
}

cfg_if! {
    if #[cfg(feature = "postgres")] {
        use diesel::pg::PgConnection;
        impl<'a> Into<&'a PgConnection> for TransactionWrapper<'a, PgConnection> {
            fn into(self) -> &'a PgConnection {
                self.0
            }
        }
    }
}

cfg_if! {
    if #[cfg(feature = "sqlite")] {
        use diesel::sqlite::SqliteConnection;
        impl<'a> Into<&'a SqliteConnection> for TransactionWrapper<'a, SqliteConnection> {
            fn into(self) -> &'a SqliteConnection {
                self.0
            }
        }
    }
}

#[macro_export]
macro_rules! load {
    ( One, $($tt:tt)+ ) => { load! { @ $($tt)+ } };
    ( OneOrNone, $($tt:tt)+ ) => { load! { @ $($tt)+ } };
    ( Many, $($tt:tt)+ ) => { load! { @ $($tt)+ } };
    ( AtLeastOne, $($tt:tt)+ ) => { load! { @ $($tt)+ } };

    (
        @
        $record_ty:ty,
        $rel_field_names:ident,
        $rel_field_ty:ty,
        $client:ident,
        $schema:ident,
        $rel_field_name:ident$(,)?
    ) => {
        {
            PreSortValues::from_unique_values_and_keys(
                $schema::table
                    .filter($schema::deleted_at.is_null())
                    .filter($schema::$rel_field_name.eq_any($rel_field_names.clone()))
                    .load::<$record_ty>($client)?,
                &$rel_field_names,
                |x| &x.$rel_field_name,
            )
        }
    };
}

#[macro_export]
macro_rules! load_all {
    (
        $client:ident,
        $schema:ident$(,)?
    ) => {
        {
            PreSortValues::from(
                $schema::table
                    .filter($schema::deleted_at.is_null())
                    .load($client)
                    .map_err(<Self as hex_arch::BaseRepository>::Error::from)?
            )
        }
    };
}

#[macro_export]
macro_rules! load_by {
    (
        $record_ty:ty,
        $rel_record_ty:ty,
        $rel_field_names:ident,
        $rel_field_ty:ty,
        $client:ident,
        $schema:ident,
        $rel_schema:ident,
        $rel_field_name:ident
        $(,$(
            $order_by_field:ident,
            $order_by_ordering:tt $(,)?
        )?)?
    ) => {
        {
            PreSortValues::from_unique_values_and_keys(
                $schema::table
                    .inner_join($rel_schema::table)
                    .filter($schema::deleted_at.is_null())
                    .filter($rel_schema::deleted_at.is_null())
                    .filter($rel_schema::$rel_field_name.eq_any($rel_field_names.clone()))
                    $($(
                        .order_by($schema::$order_by_field.$order_by_ordering())
                    )?)?
                    .load::<($record_ty, $rel_record_ty)>($client)?
                    .into_iter()
                    .map(|(record, rel_record)| (record, rel_record.$rel_field_name))
                    .collect(),
                &$rel_field_names,
                |x| &x.1,
            )
        }
    };
}

#[macro_export]
macro_rules! load_keys_by {
    (
        $field_ty:ty,
        $rel_field_names:ident,
        $client:ident,
        $schema:ident,
        $rel_schema:ident,
        $field_name:ident,
        $rel_field_name:ident$(,)?
    ) => {
        {
            $schema::table
                .select($schema::$field_name)
                .inner_join($rel_schema::table)
                .filter($schema::deleted_at.is_null())
                .filter($rel_schema::deleted_at.is_null())
                .filter($rel_schema::$rel_field_name.eq_any($rel_field_names.clone()))
                .load::<$field_ty>($client)?
        }
    };
}


#[macro_export]
macro_rules! insert {
    ($name:ident, $schema:ident, $posts:ident, $client:ident) => {
        paste! {
            {
                let records: Vec<[<Db $name>]> = diesel::insert_into($schema::table)
                    .values(&$posts)
                    .get_results($client)
                    .map_err(<Self as hex_arch::BaseRepository>::Error::from)?;

                diesel::insert_into([<audit_ $schema>]::table)
                    .values(&records.iter().map(|record| record.into()).collect::<Vec<[<DbAudit $name Post>]>>())
                    .execute($client)
                    .map_err(<Self as hex_arch::BaseRepository>::Error::from)?;

                records
            }
        }
    };
}

#[macro_export]
macro_rules! update {
    ($name:ident, $schema:ident, $patches:ident, $client:ident) => {
        paste! {
            {
                let records: Vec<[<Db $name>]> = $patches
                    .into_iter()
                    .map(|patch| patch.save_changes($client))
                    .collect::<Result<_, _>>()?;

                diesel::insert_into([<audit_ $schema>]::table)
                    .values(&records.iter().map(|record| record.into()).collect::<Vec<[<DbAudit $name Post>]>>())
                    .execute($client)
                    .map_err(<Self as hex_arch::BaseRepository>::Error::from)?;

                records
            }
        }
    };
}

#[macro_export]
macro_rules! delete {
    ($name:ident, $schema:ident, $column:ident, $client:ident, $expr:expr) => {
        paste! {
            {
                let records: Vec<[<Db $name>]> = diesel::update($schema::table.filter($schema::$column.eq_any($expr)))
                    .set($schema::deleted_at.eq(chrono::offset::Utc::now().naive_utc()))
                    .get_results($client)?;

                diesel::insert_into([<audit_ $schema>]::table)
                    .values(&records.iter().map(|record| record.into()).collect::<Vec<[<DbAudit $name Post>]>>())
                    .execute($client)
                    .map_err(<Self as hex_arch::BaseRepository>::Error::from)?;

                records
            }
        }
    };
}

#[macro_export]
macro_rules! delete_by {
    ($name:ident, $schema:ident, $rel_schema:ident, $column:ident, $client:ident, $expr:expr) => {
        paste! {
            {
                let matching_rows = $schema::table
                    .inner_join($rel_schema::table)
                    .filter($rel_schema::$column.eq_any($expr));

                let records: Vec<[<Db $name>]> = diesel::update($schema::table)
                    .filter($schema::id.eq_any(matching_rows.select($schema::id)))
                    .set($schema::deleted_at.eq(chrono::offset::Utc::now().naive_utc()))
                    .get_results($client)?;

                diesel::insert_into([<audit_ $schema>]::table)
                    .values(&records.iter().map(|record| record.into()).collect::<Vec<[<DbAudit $name Post>]>>())
                    .execute($client)
                    .map_err(<Self as hex_arch::BaseRepository>::Error::from)?;

                records
            }
        }
    };
}
