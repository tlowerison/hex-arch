#[macro_use] extern crate paste;

macro_rules! transpose {
    ($num:literal, $($generic:ident)*) => {
        paste! {
            pub fn [<transpose_ $num>]<$([<$generic:upper>]),*>(tuples: Vec<($([<$generic:upper>]),*)>) -> ($(Vec<[<$generic:upper>]>),*) {
                $(
                    let mut [<$generic _items>]: Vec<[<$generic:upper>]> = Vec::with_capacity(tuples.len());
                )*
                for ($($generic),*) in tuples.into_iter() {
                    $([<$generic _items>].push($generic);)*
                }
                ($([<$generic _items>]),*)
            }
        }
    };
}

transpose!(2, a b);
transpose!(3, a b c);
transpose!(4, a b c d);
transpose!(5, a b c d e);
transpose!(6, a b c d e f);
