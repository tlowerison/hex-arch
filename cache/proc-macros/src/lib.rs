#[macro_use]
extern crate fields;
extern crate proc_macro;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::collections::HashMap;
use syn::{braced, Ident, parenthesized, Token, Type};
use syn::parse::{Parse, ParseStream};
use syn::parse_macro_input;
use syn::punctuated::Punctuated;

struct CachesInput {
    namespace: Ident,
    adaptor_trait_bound: Type,
    key_value_client_trait_bound: Type,
    use_uuid_input: UseUuidInput,
    cache_inputs: HashMap<Ident, CacheInput>,
}

struct UseUuidInput {
    name_ident: Ident,
    uuid_ident: Ident,
    key_value_client_ident: Ident,
    fn_body: TokenStream2,
}

struct CacheInput {
    plural: Ident,
    key: KeyInput,
    value: Type,
    dependents: Vec<Ident>,
    load_values_fn: TokenStream2,
}

struct KeyInput {
    ty: Type,
    accessor_ident: Ident,
    accessor: TokenStream2,
}

impl Parse for CachesInput {
    fn parse(parse_stream: ParseStream) -> syn::Result<CachesInput> {
        let fields: CachesInputFields = parse_stream.parse()?;

        Ok(CachesInput {
            namespace: fields.namespace,
            adaptor_trait_bound: fields.adaptor_trait_bound,
            key_value_client_trait_bound: fields.key_value_client_trait_bound,
            use_uuid_input: fields.use_uuid,
            cache_inputs: fields.entities
                .into_iter()
                .map(|cache_input| (cache_input.plural.clone(), cache_input))
                .collect(),
        })
    }
}

fields!(CachesInput {
    namespace!: input -> Ident { input.parse()? },
    adaptor_trait_bound!: input -> Type { input.parse()? },
    key_value_client_trait_bound!: input -> Type { input.parse()? },
    use_uuid!: input -> UseUuidInput {
        let in_braces;
        braced!(in_braces in input);

        let _vert: Token![|] = in_braces.parse()?;
        let name_ident: Ident = in_braces.parse()?;
        let _comma: Token![,] = in_braces.parse()?;
        let uuid_ident: Ident = in_braces.parse()?;
        let _comma: Token![,] = in_braces.parse()?;
        let key_value_client_ident: Ident = in_braces.parse()?;
        let _vert: Token![|] = in_braces.parse()?;
        let fn_body: TokenStream2 = in_braces.parse()?;

        UseUuidInput { name_ident, uuid_ident, key_value_client_ident, fn_body }
    },
    entities!: input -> Vec<CacheInput> {
        let in_braces;
        braced!(in_braces in input);
        let entities: Punctuated<CacheInput, Token![,]> = in_braces.parse_terminated(CacheInput::parse)?;
        entities.into_iter().collect()
    }
});

impl Parse for CacheInput {
    fn parse(parse_stream: ParseStream) -> syn::Result<CacheInput> {
        let plural: Ident = parse_stream.parse()?;
        let _colon: Token![:] = parse_stream.parse()?;

        let in_braces;
        braced!(in_braces in parse_stream);

        let CacheInputFields { key, value, dependents, load: load_values_fn } = in_braces.parse()?;

        Ok(CacheInput {
            plural,
            key,
            value,
            dependents,
            load_values_fn,
        })
    }
}

fields!(CacheInput {
    key!: input -> KeyInput {
        let in_braces;
        braced!(in_braces in input);
        in_braces.parse()?
    },
    value!: input -> Type { input.parse()? },
    dependents!: input -> Vec<Ident> {
        let in_parens;
        parenthesized!(in_parens in input);
        let dependents: Punctuated<Ident, Token![,]> = in_parens.parse_terminated(Ident::parse)?;
        dependents.into_iter().collect()
    },
    load!: input -> TokenStream2 {
        let in_braces;
        braced!(in_braces in input);
        in_braces.parse()?
    },
});

impl Parse for KeyInput {
    fn parse(parse_stream: ParseStream) -> syn::Result<KeyInput> {
        let KeyInputFields { ty, accessor: (accessor_ident, accessor) } = parse_stream.parse()?;
        Ok(KeyInput { ty, accessor_ident, accessor })
    }
}

fields!(KeyInput {
    ty!: input -> Type { input.parse()? },
    accessor!: input -> (Ident, TokenStream2) {
        let in_braces;
        braced!(in_braces in input);
        let _vert: Token![|] = in_braces.parse()?;
        let accessor_ident: Ident = in_braces.parse()?;
        let _vert: Token![|] = in_braces.parse()?;
        let accessor: TokenStream2 = in_braces.parse()?;
        (accessor_ident, accessor)
    },
});

impl CacheInput {
    fn ty(&self, namespace: &Ident) -> TokenStream2 {
        let plural = &self.plural;
        let key = &self.key.ty;
        let value = &self.value;

        quote! { [<#namespace Cache>]<'a, #key, #value, [<#namespace Cache #plural>]> }
    }
}

#[proc_macro]
pub fn caches(token_stream: TokenStream) -> TokenStream {
    let caches_input = parse_macro_input!(token_stream as CachesInput);

    for cache_input in caches_input.cache_inputs.values() {
        for dependent in cache_input.dependents.iter() {
            if *dependent == cache_input.plural {
                return syn::parse::Error::new_spanned(dependent, "cache key cannot depend on itself (no self-cycles allowed)").into_compile_error().into();
            }
            if !caches_input.cache_inputs.contains_key(dependent) {
                return syn::parse::Error::new_spanned(
                    dependent,
                    "unknown cache key, dependents keys must be specified as top-level keys in the caches macro",
                ).into_compile_error().into();
            }
        }
    }

    let namespace = &caches_input.namespace;
    let adaptor_trait_bound = &caches_input.adaptor_trait_bound;
    let key_value_client_trait_bound = &caches_input.key_value_client_trait_bound;
    let load_values_fns: Vec<_> = caches_input.cache_inputs.values().map(|cache_input| cache_input.load_values_fn.clone()).collect();
    let plurals: Vec<_> = caches_input.cache_inputs.values().map(|cache_input| cache_input.plural.clone()).collect();
    let tys: Vec<_> = caches_input.cache_inputs.values().map(|cache_input| cache_input.ty(&namespace)).collect();
    let keys: Vec<_> = caches_input.cache_inputs.values().map(|cache_input| &cache_input.key.ty).collect();
    let key_accessor_idents: Vec<_> = caches_input.cache_inputs.values().map(|cache_input| &cache_input.key.accessor_ident).collect();
    let key_accessors: Vec<_> = caches_input.cache_inputs.values().map(|cache_input| &cache_input.key.accessor).collect();
    let values: Vec<_> = caches_input.cache_inputs.values().map(|cache_input| &cache_input.value).collect();

    let use_uuid_name_ident = &caches_input.use_uuid_input.name_ident;
    let use_uuid_uuid_ident = &caches_input.use_uuid_input.uuid_ident;
    let use_uuid_key_value_client_ident = &caches_input.use_uuid_input.key_value_client_ident;
    let use_uuid_fn_body = &caches_input.use_uuid_input.fn_body;

    let dependent_plurals: Vec<Vec<_>> = caches_input.cache_inputs
        .values()
        .map(|cache_input| cache_input.dependents.clone())
        .collect();

    let tokens = quote! {
        hex_arch_paste! {
            pub struct [<#namespace Cache>]<'a, Key, Value, LV> {
                name: [String; 1],
                uuid: String,
                values: std::collections::HashMap<Key, std::sync::Arc<Value>>,
                visited: bool,
                visitables: Vec<std::sync::Weak<std::sync::RwLock<dyn Visitable + 'a>>>,
                _lv: std::marker::PhantomData<LV>,
            }

            pub trait [<#namespace LoadValues>] {
                type Key;
                type Value;
                fn load_values<Adaptor: #adaptor_trait_bound>(client: <Adaptor as BaseRepository>::Client<'_>) -> Option<Vec<Self::Value>>;
                fn accessor(value: &Self::Value) -> Self::Key;
            }

            impl<'a, Key: Send + Sync, Value: Send + Sync, LV: Send + Sync> [<#namespace Cache>]<'a, Key, Value, LV> {
                pub fn new(name: &str) -> Self {
                    [<#namespace Cache>] {
                        name: [format!("cache:{}", name)],
                        uuid: default_cache_uuid(),
                        values: std::collections::HashMap::default(),
                        visited: false,
                        visitables: Vec::with_capacity(0),
                        _lv: std::marker::PhantomData,
                    }
                }

                pub fn set_visitables(&mut self, visitables: Vec<std::sync::Weak<std::sync::RwLock<dyn Visitable + 'a>>>) {
                    self.visitables = visitables;
                }

                pub fn name(&self) -> &str {
                    &self.name[0]
                }

                pub fn uuid(&self) -> &str {
                    &self.uuid
                }

                fn is_cache_valid(&self, current_uuid: &Option<String>) -> bool {
                    match current_uuid.as_ref() {
                        Some(current_uuid) => *current_uuid == self.uuid,
                        None => false,
                    }
                }
            }

            impl<'a, Key, Value, LV> [<#namespace Cache>]<'a, Key, Value, LV> {
                pub fn use_uuid<KeyValueClient>(&self, key_value_client: &mut KeyValueClient) -> Result<(), CacheError>
                where
                    KeyValueClient: #key_value_client_trait_bound,
                {
                    use_uuid_util::<KeyValueClient>(&self.name, &self.uuid, key_value_client)
                }
            }

            fn use_uuid_util<KeyValueClient>(#use_uuid_name_ident: &[String; 1], #use_uuid_uuid_ident: &str, #use_uuid_key_value_client_ident: &mut KeyValueClient) -> Result<(), CacheError>
            where
                KeyValueClient: #key_value_client_trait_bound,
            {
                { #use_uuid_fn_body }.map_err(|_| CacheError::CouldNotConfirmCacheValidity)
            }

            impl<
                'a,
                Key: Clone + Eq + std::hash::Hash + Send + Sync,
                Value: Send + Sync,
                LV: [<#namespace LoadValues>]<Key = Key, Value = Value> + Send + Sync,
            > [<#namespace Cache>]<'a, Key, Value, LV> {

                pub fn get<Q: ?Sized, Adaptor: #adaptor_trait_bound, KeyValueClient: #key_value_client_trait_bound>(
                    &mut self,
                    key: &Q,
                    (client, key_value_client): (<Adaptor as BaseRepository>::Client<'_>, &mut KeyValueClient),
                ) -> Option<std::sync::Arc<Value>>
                where
                    Key: std::borrow::Borrow<Q>,
                    Q: std::hash::Hash + Eq,
                {
                    self.update::<Adaptor, KeyValueClient>((client, key_value_client)).ok()?;
                    self.values.get(key).map(|value| value.clone())
                }

                pub fn iter<Adaptor: #adaptor_trait_bound, KeyValueClient: #key_value_client_trait_bound>(
                    &mut self,
                    (client, key_value_client): (<Adaptor as BaseRepository>::Client<'_>, &mut KeyValueClient),
                ) -> Result<std::collections::hash_map::Values<'_, Key, std::sync::Arc<Value>>, CacheError> {
                    self.update::<Adaptor, KeyValueClient>((client, key_value_client))?;
                    Ok(self.values.values())
                }

                pub fn into_iter<Adaptor: #adaptor_trait_bound, KeyValueClient: #key_value_client_trait_bound>(
                    mut self,
                    (client, key_value_client): (<Adaptor as BaseRepository>::Client<'_>, &mut KeyValueClient),
                ) -> Result<std::collections::hash_map::IntoValues<Key, std::sync::Arc<Value>>, CacheError> {
                    self.update::<Adaptor, KeyValueClient>((client, key_value_client))?;
                    Ok(self.values.into_values())
                }

                fn update<Adaptor: #adaptor_trait_bound, KeyValueClient: #key_value_client_trait_bound>(
                    &mut self,
                    (client, key_value_client): (<Adaptor as BaseRepository>::Client<'_>, &mut KeyValueClient),
                ) -> Result<(), CacheError> {
                    let current_uuid: Option<String> = key_value_client.get(&self.name).map_err(|_| CacheError::CouldNotConfirmCacheValidity)?;

                    if !self.is_cache_valid(&current_uuid) {
                        self.load_values::<Adaptor>(client)?;
                        if let Some(current_uuid) = current_uuid {
                            self.uuid = current_uuid;
                        } else {
                            self.use_uuid(key_value_client)?;
                        }
                    }
                    Ok(())
                }

                fn load_values<Adaptor: #adaptor_trait_bound>(
                    &mut self,
                    client: <Adaptor as BaseRepository>::Client<'_>,
                ) -> Result<(), CacheError> {
                    self.values = LV::load_values::<Adaptor>(client)
                        .ok_or(CacheError::CouldNotLoadResources)?
                        .into_iter()
                        .map(|value| (LV::accessor(&value), std::sync::Arc::new(value)))
                        .collect();
                    Ok(())
                }
            }


            pub trait Visitable: Send + Sync {
                fn visited(&self) -> bool;
                fn visit(&mut self);
                fn unvisit(&mut self);
                fn reload(&mut self);
            }

            impl<'a, Key: Send + Sync, Value: Send + Sync, LV: Send + Sync> Visitable for [<#namespace Cache>]<'a, Key, Value, LV> {
                fn visited(&self) -> bool {
                    self.visited
                }

                fn visit(&mut self) {
                    self.visited = true;
                }

                fn unvisit(&mut self) {
                    self.visited = false;
                }

                fn reload(&mut self) {
                    self.visit();
                    self.uuid = default_cache_uuid();
                    for visitable_rw_lock_ptr in self.visitables.iter_mut() {
                        let visitable_rw_lock = visitable_rw_lock_ptr.upgrade().unwrap();
                        let visited = {
                            let visitable = visitable_rw_lock.read().unwrap();
                            visitable.visited()
                        };
                        if !visited {
                            let mut visitable = visitable_rw_lock.write().unwrap();
                            visitable.reload();
                        }
                    }
                }
            }


            pub struct [<#namespace Caches>]<'a> {
                #(
                    #plurals: std::sync::Arc<std::sync::RwLock<#tys>>,
                )*
            }

            #(
                #[allow(non_camel_case_types)]
                pub struct [<#namespace Cache #plurals>];

                impl [<#namespace LoadValues>] for [<#namespace Cache #plurals>] {
                    type Key = #keys;
                    type Value = #values;

                    fn load_values<Adaptor: #adaptor_trait_bound>(client: <Adaptor as BaseRepository>::Client<'_>) -> Option<Vec<Self::Value>> {
                        #load_values_fns
                    }

                    fn accessor(#key_accessor_idents: &Self::Value) -> Self::Key {
                        #key_accessors
                    }
                }
            )*

            impl<'a> [<#namespace Caches>]<'a> {
                pub fn new() -> [<#namespace Caches>]<'a> {
                    #(
                        let #plurals = std::sync::Arc::new(std::sync::RwLock::new([<#namespace Cache>]::new(stringify!(#plurals))));
                    )*
                    #({
                        let mut [<#plurals _val>] = #plurals.write().unwrap();
                        [<#plurals _val>].set_visitables(vec![#(
                            std::sync::Arc::downgrade(&#dependent_plurals) as std::sync::Weak::<std::sync::RwLock<dyn Visitable>>,
                        )*]);
                    })*
                    [<#namespace Caches>] {
                        #(#plurals,)*
                    }
                }

                #(
                    pub fn #plurals(&mut self) -> &mut std::sync::Arc<std::sync::RwLock<#tys>> {
                        &mut self.#plurals
                    }
                )*

                #(
                    pub fn [<reload_ #plurals>]<Adaptor: #adaptor_trait_bound, KeyValueClient: #key_value_client_trait_bound>(
                        &mut self,
                        clients: (<Adaptor as BaseRepository>::Client<'_>, &mut KeyValueClient),
                    ) -> Result<(), CacheError> {
                        {
                            let mut #plurals = self.#plurals.write().unwrap();
                            #plurals.reload();
                        }
                        self.unvisit_all::<Adaptor, KeyValueClient>(clients)
                    }
                )*

                fn unvisit_all<Adaptor: #adaptor_trait_bound, KeyValueClient: #key_value_client_trait_bound>(
                    &mut self,
                    (client, key_value_client): (<Adaptor as BaseRepository>::Client<'_>, &mut KeyValueClient),
                ) -> Result<(), CacheError> {

                    #({
                        let mut #plurals = self.#plurals.write().unwrap();
                        if #plurals.visited() {
                            #plurals.unvisit();
                            #plurals.update::<Adaptor, KeyValueClient>((client, key_value_client))?;
                        }
                    })*
                    Ok(())
                }
            }
        }
    };
    tokens.into()
}
