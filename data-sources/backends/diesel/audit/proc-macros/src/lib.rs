#[macro_use] extern crate quote;
extern crate proc_macro;

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use syn::{DeriveInput, Ident};

#[proc_macro_derive(Audit)]
pub fn derive_audit(tokens: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(tokens).unwrap();

    let name = format!("{}", &ast.ident);
    let snake_name = format!("{}", name).to_case(Case::Snake);
    if &snake_name[..3] != "db_" {
        panic!("Audit tables expect a naming schema of Db{{}}, found {}", name);
    }

    let name = format_ident!("{}", &name[2..]);
    let table_name = format_ident!("{}", format!("{}", &snake_name[3..]).to_case(Case::Snake));
    let stringified_audit_table_name = format!("audit_{}", table_name).to_case(Case::Snake);

    let (mut field_names, mut field_types): (Vec<_>, Vec<_>) = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| (field.ident.as_ref().unwrap().clone(), field.ty.clone()))
                .unzip(),
            _ => panic!("Audit can only be derived on struct data types with named fields at this time."),
        },
        _ => panic!("Audit can only be derived on struct data types at this time."),
    };

    let primary_key_name = get_primary_key(&ast);

    let mut primary_key_index: isize = -1;
    for (index, field_name) in field_names.iter().enumerate() {
        if *field_name == primary_key_name {
            primary_key_index = index as isize;
            break;
        }
    }

    let primary_key_index = if primary_key_index < 0 {
        panic!("Audit could not be derived for primary key `{}`, field not found in `{}`", primary_key_name, name);
    } else {
        primary_key_index as usize
    };

    let primary_key_type = field_types[primary_key_index].clone();

    let mut into_field_names = field_names.clone();
    let mut into_field_types = field_types.clone();
    into_field_names.remove(primary_key_index);
    into_field_types.remove(primary_key_index);

    field_names.insert(primary_key_index + 1, format_ident!("{}_{}", table_name, primary_key_name));
    field_types.insert(primary_key_index + 1, primary_key_type.clone());

    let tokens = quote! {
        audit_paste! {
            #[derive(Associations, Clone, Debug, Deserialize, Identifiable, Queryable, Serialize)]
            #[table_name = #stringified_audit_table_name]
            #[primary_key(#primary_key_name)]
            pub struct [<DbAudit #name>] {
                #(
                    pub #field_names: #field_types,
                )*
                pub created_at: chrono::NaiveDateTime,
            }

            impl AsRef<#primary_key_type> for [<DbAudit #name>] {
                fn as_ref(&self) -> &#primary_key_type {
                    &self.#primary_key_name
                }
            }

            impl Into<#primary_key_type> for [<DbAudit #name>] {
                fn into(self) -> #primary_key_type {
                    self.#primary_key_name
                }
            }

            impl Into<[<Audit #name>]> for [<DbAudit #name>] {
                fn into(self) -> [<Audit #name>] {
                    [<Audit #name>] {
                        #primary_key_name: self.#primary_key_name,
                        #table_name: [<Db #name>] {
                            #primary_key_name: self.[<#table_name _ #primary_key_name>],
                            #(#into_field_names: self.#into_field_names,)*
                        }.into(),
                        created_at: self.created_at,
                    }
                }
            }

            impl From<&[<Db #name>]> for [<DbAudit #name Post>] {
                fn from(record: &[<Db #name>]) -> [<DbAudit #name Post>] {
                    [<DbAudit #name Post>] {
                        [<#table_name _ #primary_key_name>]: record.#primary_key_name.clone(),
                        #(#into_field_names: record.#into_field_names.clone(),)*
                        created_at: chrono::offset::Utc::now().naive_utc(),
                    }
                }
            }

            impl From<[<Db #name>]> for [<DbAudit #name Post>] {
                fn from(record: [<Db #name>]) -> [<DbAudit #name Post>] {
                    [<DbAudit #name Post>] {
                        [<#table_name _ #primary_key_name>]: record.#primary_key_name,
                        #(#into_field_names: record.#into_field_names,)*
                        created_at: chrono::offset::Utc::now().naive_utc(),
                    }
                }
            }


            #[derive(Clone, Debug, Deserialize, Insertable, Serialize)]
            #[table_name = #stringified_audit_table_name]
            pub struct [<DbAudit #name Post>] {
                pub [<#table_name _ #primary_key_name>]: #primary_key_type,
                #(
                    pub #into_field_names: #into_field_types,
                )*
                pub created_at: chrono::NaiveDateTime,
            }
        }
    };

    tokens.into()
}

fn get_primary_key(ast: &DeriveInput) -> Ident {
    ast.attrs
        .iter()
        .find(|attr| {
            if let Some(attr_ident) = attr.path.get_ident() {
                if &*format!("{}", attr_ident) == "primary_key" {
                    return true
                }
            }
            false
        })
        .map(|attr| attr.parse_args().unwrap())
        .unwrap_or_else(|| format_ident!("id"))
}
