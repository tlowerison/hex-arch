extern crate proc_macro;

use proc_macro::TokenStream;
use repositories_core::RepositoriesInput;
use syn::parse_macro_input;

///
/// Generates a set of "repository" traits from a list of entities and their relations.
/// Each trait specifies a collection of functions which must be implemeneted for loading
/// entities by their keys and loading entities by their relations' keys. Building on top
/// of those load functions, a set of "get" and "get_by" functions are implemented on each
/// entity type which will retrieve those entities and their relations in a nested structure.
///
/// # Examples
///
/// ```
/// #[derive(Clone, Debug)]
/// pub struct Restaurant {
///     pub id: i32,
///     pub name: String,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct Menu {
///     pub id: i32,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct MenuItem {
///     pub id: i32,
///     pub name: String,
///     pub description: Option<String>,
///     pub cost: f64,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct Table {
///     pub id: i32,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct Seat {
///     pub id: i32,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct TableAssignment {
///     pub id: i32,
///     pub start_date_time: chrono::NaiveDateTime,
///     pub end_date_time: Option<chrono::NaiveDateTime>,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct SeatingAssignment {
///     pub id: i32,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct Guest {
///     pub id: i32,
///     pub name: String,
/// }
///
/// repositories! {
///     // {sync,unsync} describes whether to use Arc or Rc wrappers for underlying use of smart pointers
///     // `Restaurant` is this repository set's namespace
///     sync Restaurant {
///         Guest RW {
///             key: (i32, id),
///             relations: {
///                 seating_assignments: (SeatingAssignment, Many),
///             },
///         },
///         Restaurant RW {
///             key: (i32, id),
///             relations: {
///                 menu: (Menu, One),
///                 tables: (Table, Many),
///             },
///         },
///         Menu RW {
///             key: (i32, id),
///             relations: {
///                 restaurant: (Restaurant, One),
///                 items: (MenuItem, AtLeastOne),
///             },
///         },
///         Seat RW {
///             key: (i32, id),
///             relations: {
///                 table: (Table, One),
///                 seating_assignments: (SeatingAssignment, Many),
///             },
///         },
///         SeatingAssignment RW {
///             key: (i32, id),
///             relations: {
///                 seat: (Seat, One),
///                 guest: (Guest, One),
///                 table_assignment: (TableAssignment, One),
///             },
///         },
///         Table RW {
///             key: (i32, id),
///             relations: {
///                 restaurant: (Restaurant, One),
///                 seats: (Seat, Many),
///                 table_assignments: (TableAssignment, Many),
///             },
///         },
///         TableAssignment RW {
///             key: (i32, id),
///             relations: {
///                 table: (Table, One),
///                 seating_assignments: (SeatingAssignment, Many),
///             },
///         },
///     }
/// }
///
/// fn can_seat_guests_at_table<Adaptor: RestaurantReadWriteRepository>(
///     table_id: i32,
///     num_guests: usize,
///     client: <Adaptor as BaseRepository>::Client<'_>,
/// ) -> Result<bool, <Adaptor as BaseRepository>::Error> {
///     let table = Table::get(table_id)
///         .load_seats()
///         .load_table_assignments()
///         .run(client)?;
///
///     // Does this table have enough seats for this number of guests?
///     if table.relations.seats.unwrap().len() < num_guests {
///         return false;
///     }
///
///     // Are there any table assignments already at this table currently?
///     if table.relations.table_assignments
///         .unwrap()
///         .find(|table_assignment| table_assignment.end_date_time.is_none())
///         .is_some()
///     {
///         return false;
///     }
///     // (^NOTE: This isn't exactly a performant query where every time we want
///     // to check if the table is open we're loading every table assignment it's
///     // ever had; work is in progress to provide support for chaining where / query clauses
///     // on the provided get / load relations functions)
///
///     return true;
/// }
/// ```
#[proc_macro]
pub fn repositories(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as RepositoriesInput);
    repositories_core::repositories(input, false).into()
}

#[proc_macro]
pub fn print_repositories(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as RepositoriesInput);
    repositories_core::repositories(input, true).into()
}
