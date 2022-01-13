use convert_case::{Case, Casing};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, parenthesized, token::Paren, Ident, Token, Type};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Cardinality {
    One,
    OneOrNone,
    AtLeastOne,
    Many,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Mutability {
    R,
    RW,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Syncability {
    Sync,
    Unsync,
}

#[derive(Clone)]
pub struct RepositoriesInput {
    pub name: Option<Ident>,
    pub load_relations_trait_mod: Ident,
    pub repositories: Vec<RepositoryInput>,
}

#[derive(Clone)]
pub struct RepositoryIdents {
    pub ty: Ident,
    pub singular: Ident,
    pub plural: Ident,
}

#[derive(Clone)]
pub struct KeyIdents {
    pub ty: Type,
    pub singular: Ident,
    pub plural: Ident,
}

#[derive(Clone)]
pub struct RepositoryInput {
    pub idents: RepositoryIdents,
    pub key: KeyIdents,
    pub mutability: Mutability,
    pub read_repositories: Ident,
    pub relations: Vec<RelationInput>,
    pub load_bys: Vec<LoadByInput>,
    pub syncability: Option<Syncability>,
}

#[derive(Clone)]
pub struct RelationIdents {
    pub ty: Ident,
    pub snake: Ident,
    pub pascal: Ident,
    pub plural: Ident,
}

#[derive(Clone)]
pub struct RelationInput {
    pub idents: RelationIdents,
    pub cardinality: Cardinality,
    pub syncability: Option<Syncability>,
}

#[derive(Clone)]
pub struct LoadByIdents {
    pub singular: Ident,
    pub plural: Ident,
}

#[derive(Clone)]
pub struct LoadByInput {
    pub idents: LoadByIdents,
    pub ty: Type,
    pub cardinality: Cardinality,
}

fields! {
    RepositoryInput {
        key!: input -> KeyIdents {
            let in_paren;
            parenthesized!(in_paren in input);
            let ty: Type = in_paren.parse()?;
            let _comma: Token![,] = in_paren.parse()?;
            let singular: Ident = in_paren.parse()?;
            let comma: Option<Token![,]> = in_paren.parse().ok();

            let plural: Ident = if let Some(_) = comma {
                in_paren.parse()?
            } else {
                format_ident!("{}s", singular)
            };

            KeyIdents { ty, singular, plural }
        },
        relations?: input -> Vec<RelationInput> {
            let in_brace;
            braced!(in_brace in input);
            let relations: Punctuated<RelationInput, Token![,]> = in_brace.parse_terminated(RelationInput::parse)?;
            relations.into_iter().collect()
        },
        load_by?: input -> Vec<LoadByInput> {
            let in_brace;
            braced!(in_brace in input);
            let load_bys: Punctuated<LoadByInput, Token![,]> = in_brace.parse_terminated(LoadByInput::parse)?;
            load_bys.into_iter().collect()
        },
    }
}

impl From<&Ident> for Cardinality {
    fn from(ident: &Ident) -> Cardinality {
        match &*format!("{}", ident) {
            "One" => Cardinality::One,
            "OneOrNone" => Cardinality::OneOrNone,
            "AtLeastOne" => Cardinality::AtLeastOne,
            "Many" => Cardinality::Many,
            other => panic!("unrecognized relation cardinality: expected one of {{One, OneOrNone, Many, AtLeastOne}}, received {}", other),
        }
    }
}

impl From<Ident> for Cardinality {
    fn from(ident: Ident) -> Cardinality {
        Cardinality::from(&ident)
    }
}

impl ToTokens for Cardinality {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Cardinality::One => tokens.append(format_ident!("One")),
            Cardinality::OneOrNone => tokens.append(format_ident!("OneOrNone")),
            Cardinality::AtLeastOne => tokens.append(format_ident!("AtLeastOne")),
            Cardinality::Many => tokens.append(format_ident!("Many")),
        }
    }
}

impl From<&Ident> for Mutability {
    fn from(ident: &Ident) -> Mutability {
        let string = format!("{}", ident);
        match &*string {
            "R" => Mutability::R,
            "RW" => Mutability::RW,
            other => panic!(
                "unrecognized mutability: expected one of {{R, RW}}, received {}",
                other
            ),
        }
    }
}

impl From<Ident> for Mutability {
    fn from(ident: Ident) -> Mutability {
        Mutability::from(&ident)
    }
}

impl From<&Ident> for Syncability {
    fn from(ident: &Ident) -> Syncability {
        let string = format!("{}", ident);
        match &*string {
            "sync" => Syncability::Sync,
            "unsync" => Syncability::Unsync,
            other => panic!(
                "unrecognized syncability: expected one of {{sync, unsync}}, received {}",
                other
            ),
        }
    }
}

impl From<Ident> for Syncability {
    fn from(ident: Ident) -> Syncability {
        Syncability::from(&ident)
    }
}

impl Parse for RepositoriesInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_brace;

        let syncability: Syncability = Syncability::from(&input.parse()?);
        let name: Option<Ident> = input.parse().ok();

        braced!(in_brace in input);
        let repositories: Punctuated<RepositoryInput, Token![,]> =
            in_brace.parse_terminated(RepositoryInput::parse)?;

        let mut input = RepositoriesInput {
            load_relations_trait_mod: name
                .as_ref()
                .map(|name| format_ident!("{}_load_relation_traits", format_ident!("{}", format!("{}", name).to_case(Case::Snake))))
                .unwrap_or(format_ident!("load_relation_traits")),
            name,
            repositories: repositories
                .into_iter()
                .map(|repository| RepositoryInput {
                    syncability: Some(syncability.clone()),
                    relations: repository
                        .relations
                        .into_iter()
                        .map(|relation| RelationInput {
                            syncability: Some(syncability.clone()),
                            ..relation
                        })
                        .collect(),
                    ..repository
                })
                .collect(),
        };

        let read_repositories = input
            .name
            .as_ref()
            .map(|name| format_ident!("{}ReadRepository", name))
            .unwrap_or_else(|| format_ident!("ReadRepository"));

        for repository in input.repositories.iter_mut() {
            repository.read_repositories = read_repositories.clone();
        }

        Ok(input)
    }
}

impl Parse for RepositoryInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Ident = input.parse()?;

        let idents = RepositoryIdents {
            singular: format_ident!("{}", format!("{}", ty).to_case(Case::Snake)),
            plural: if input.peek(Paren) {
                let in_paren;
                let _paren: Paren = parenthesized!(in_paren in input);
                in_paren.parse()?
            } else {
                format_ident!("{}", format!("{}s", ty).to_case(Case::Snake))
            },
            ty,
        };

        let mutability: Mutability = Mutability::from(&input.parse()?);

        let in_brace;
        braced!(in_brace in input);

        let fields: RepositoryInputFields = in_brace.parse()?;

        Ok(RepositoryInput {
            idents,
            mutability,
            syncability: None,
            read_repositories: format_ident!("_"), // placeholder
            key: fields.key,
            load_bys: fields.load_by.unwrap_or(vec![]),
            relations: fields.relations.unwrap_or(vec![]),
        })
    }
}

impl Parse for RelationInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_paren;

        let snake_ident: Ident = input.parse()?;
        let plural_ident: Option<Ident> = if input.peek(Paren) {
            let in_paren;
            let _paren: Paren = parenthesized!(in_paren in input);
            Some(in_paren.parse()?)
        } else {
            None
        };

        let _colon: Token![:] = input.parse()?;
        let _paren: Paren = parenthesized!(in_paren in input);
        let ty_ident: Ident = in_paren.parse()?;
        let _comma: Token![,] = in_paren.parse()?;
        let cardinality = Cardinality::from(&in_paren.parse()?);

        Ok(RelationInput {
            syncability: None,
            idents: RelationIdents {
                ty: ty_ident,
                plural: plural_ident.unwrap_or_else(|| match cardinality {
                    Cardinality::One | Cardinality::OneOrNone => format_ident!("{}s", snake_ident),
                    _ => snake_ident.clone(),
                }),
                pascal: format_ident!("{}", format!("{}", snake_ident).to_case(Case::Pascal)),
                snake: snake_ident,
            },
            cardinality,
        })
    }
}

impl Parse for LoadByInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_paren;

        let singular: Ident = input.parse()?;
        let idents = LoadByIdents {
            plural: if input.peek(Paren) {
                let in_paren;
                parenthesized!(in_paren in input);
                in_paren.parse()?
            } else {
                format_ident!("{}", format!("{}s", singular))
            },
            singular,
        };

        let _colon: Token![:] = input.parse()?;
        parenthesized!(in_paren in input);

        let ty: Type = in_paren.parse()?;
        let _comma: Token![,] = in_paren.parse()?;

        let cardinality_span = in_paren.span();
        let cardinality = Cardinality::from(&in_paren.parse()?);

        match &cardinality {
            Cardinality::OneOrNone | Cardinality::AtLeastOne => {
                return Err(syn::Error::new(
                    cardinality_span,
                    "load by cardinality must be one of {One, Many}",
                ))
            }
            _ => {}
        };

        Ok(LoadByInput {
            idents,
            ty,
            cardinality,
        })
    }
}

impl RepositoriesInput {
    pub fn tys(&self) -> Vec<&Ident> {
        self.repositories
            .iter()
            .map(|repository| repository.ty())
            .collect()
    }
}

impl RepositoryInput {
    pub fn ty(&self) -> &Ident {
        &self.idents.ty
    }

    pub fn singular(&self) -> &Ident {
        &self.idents.singular
    }

    pub fn plural(&self) -> &Ident {
        &self.idents.plural
    }

    pub fn key_ty(&self) -> &Type {
        &self.key.ty
    }

    pub fn key_singular(&self) -> &Ident {
        &self.key.singular
    }

    pub fn key_plural(&self) -> &Ident {
        &self.key.plural
    }

    pub fn relation_tys(&self) -> Vec<&Ident> {
        self.relations
            .iter()
            .map(|relation| relation.ty())
            .collect()
    }

    pub fn relation_snakes(&self) -> Vec<&Ident> {
        self.relations
            .iter()
            .map(|relation| relation.snake())
            .collect()
    }

    pub fn relation_pascals(&self) -> Vec<&Ident> {
        self.relations
            .iter()
            .map(|relation| relation.pascal())
            .collect()
    }

    pub fn sync_ptr(&self) -> TokenStream2 {
        self.syncability.as_ref().unwrap().sync_ptr()
    }

    pub fn read_repositories(&self) -> &Ident {
        &self.read_repositories
    }

    pub fn inward_relations<'a>(
        &self,
        repositories_input: &'a RepositoriesInput,
    ) -> Vec<(&'a RelationInput, &'a RepositoryInput)> {
        repositories_input
            .repositories
            .iter()
            .map(|rep| {
                rep.relations
                    .iter()
                    .filter_map(|relation| {
                        if *relation.ty() == *self.ty() {
                            Some((relation, rep))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect()
    }
}

impl RelationInput {
    pub fn ty(&self) -> &Ident {
        &self.idents.ty
    }

    pub fn snake(&self) -> &Ident {
        &self.idents.snake
    }

    pub fn pascal(&self) -> &Ident {
        &self.idents.pascal
    }

    pub fn plural(&self) -> &Ident {
        &self.idents.plural
    }

    pub fn sync_ptr(&self) -> TokenStream2 {
        self.syncability.as_ref().unwrap().sync_ptr()
    }
}

impl LoadByInput {
    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn singular(&self) -> &Ident {
        &self.idents.singular
    }

    pub fn plural(&self) -> &Ident {
        &self.idents.plural
    }
}

impl Syncability {
    pub fn sync_ptr(&self) -> TokenStream2 {
        match &self {
            Syncability::Sync => "std::sync::Arc".parse().unwrap(),
            Syncability::Unsync => "std::rc::Rc".parse().unwrap(),
        }
    }
}
