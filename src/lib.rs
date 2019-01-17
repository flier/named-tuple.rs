//! The `named_tuple!` macro generates a `struct` that manages a set of fields in a `tuple`.
//!
//! The tuple could be access with field getter/setter at runtime.
//!
//! # Example
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use named_tuple::named_tuple;
//! named_tuple!(
//!     #[derive(Clone, Copy)]
//!     struct Human<'a> {
//!         name: &'a str,
//!         age: usize,
//!     }
//! );
//!
//! named_tuple!(
//!     #[derive(Clone, Copy)]
//!     struct Endpoint(host, port);
//! );
//!
//! fn main() {
//!     let human: Human = ("alice", 18).into();
//!
//!     assert_eq!(human.name(), "alice");
//!     assert_eq!(human.age(), 18);
//!     assert_eq!(human.field_values(), ("alice", 18));
//!
//!     let mut endpoint = Endpoint::new("localhost", 80);
//!
//!     assert_eq!(endpoint.host(), "localhost");
//!     assert_eq!(endpoint.port(), 80);
//!
//! }
//! ```
//!
//! # Visibility
//!
//! The generated struct and its associated fields are not exported out of the current module by default.
//! A definition can be exported out of the current module by adding `pub` before the `struct` keyword or the field name:
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use named_tuple::named_tuple;
//! mod example {
//!     named_tuple!(
//!         #[derive(Clone, Copy)]
//!         pub struct Human<'a> {
//!             pub name: &'a str,
//!           # pub
//!             age: usize,
//!         }
//!     );
//! }
//!
//! fn main() {
//!     let mut human = example::Human::new("alice", 18);
//!
//!     assert_eq!(human.name(), "alice");
//!     assert_eq!(human.age(), 18); // error: method `age` is private
//! }
//! ```
//!
//! # Attributes
//!
//! Attributes can be attached to the generated struct by placing them before the `struct` keyword.
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use named_tuple::named_tuple;
//! named_tuple!(
//!     #[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
//!     struct Human<'a> {
//!         name: &'a str,
//!         age: usize,
//!     }
//! );
//! # fn main() {}
//! ```
//!
//! By default, the field getter will return reference of value.
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use named_tuple::named_tuple;
//! named_tuple!(
//!     struct Endpoint(host, port);
//! );
//!
//! fn main() {
//!     let mut endpoint = Endpoint::new("localhost", 80);
//!
//!     assert_eq!(*endpoint.host(), "localhost");  // compare &&str to &str
//!     assert_eq!(*endpoint.port(), 80);           // compare &{integer} to {integer}
//!
//! }
//! ```
//!
//! You could add `#[derive(Clone, Copy)]` attribute to force it return value.
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use named_tuple::named_tuple;
//! named_tuple!(
//!     #[derive(Clone, Copy)]
//!     struct Endpoint(host, port);
//! );
//!
//! fn main() {
//!     let mut endpoint = Endpoint::new("localhost", 80);
//!
//!     assert_eq!(endpoint.host(), "localhost");
//!     assert_eq!(endpoint.port(), 80);
//!
//! }
//! ```
//!
//! # Trait implementations
//!
//! The `From`, `Into`, `Deref`, `DerefMut`, `PartialEq` and `PartialOrd` traits are implemented for the struct,
//! that make the struct could be works with the underline tuple.
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use std::net::{ToSocketAddrs};
//! # use named_tuple::named_tuple;
//! named_tuple!(
//!     #[derive(PartialEq, PartialOrd)]
//!     struct Endpoint<'a> {
//!         host: &'a str,
//!         port: u16,
//!     }
//! );
//!
//! fn main() {
//!     let endpoint: Endpoint = ("localhost", 80).into(); // From<(...)>
//!
//!     let addr = endpoint.to_socket_addrs().unwrap().collect::<Vec<_>>(); // Deref<Target=(...)>
//!
//!     if endpoint != ("localhost", 443) {         // PartialEq<(...)>
//!         if endpoint < ("localhost", 1024) {     // PartialOrd<(...)>
//!             let (host, port) = endpoint.into(); // Into<(...)>
//!         }
//!     }
//! }
//! ```
//!
//! Additional traits can be derived by providing an explicit `derive` attribute on `struct`.
//!
//! ## Serde
//!
//! For the tuple structs, the `serde` support is disabled by default,
//! you need build `named_tuple` with `serde` feature in `Cargo.toml`
//!
//! ```toml
//! [dependencies]
//! named_tuple = { version = "0.1", features = ["serde"] }
//! ```
//!
//! Then the `Serialize` and `Deserialize` will be implemented when all tuple field type implement it.
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # #[macro_use] extern crate serde_derive;
//! # use std::net::{ToSocketAddrs};
//! # use named_tuple::named_tuple;
//! # #[cfg(feature = "serde")]
//! named_tuple!(
//!     #[derive(Debug, PartialEq, Serialize, Deserialize)]
//!     struct Endpoint<'a> {
//!         host: &'a str,
//!         port: u16,
//!     }
//! );
//!
//! # #[cfg(not(feature = "serde"))]
//! # fn main() {}
//! # #[cfg(feature = "serde")]
//! fn main() {
//!     let endpoint = Endpoint::new("localhost", 80);
//!
//!     let json = serde_json::to_string(&endpoint).unwrap();
//!
//!     assert_eq!(json, "[\"localhost\",80]");
//!
//!     let endpoint2: Endpoint = serde_json::from_str(&json).unwrap();
//!
//!     assert_eq!(endpoint, endpoint2);
//! }
//! ```
//!
//! # Methods
//!
//! The following methods are defined for the generated struct:
//!
//! - `new`: constructs a new named tuple.
//! - `field_names`: returns a slice of field names.
//! - `fields`: returns a tuple of field name and value pair.
//! - `field_values`: return a tuple of field values.
//!
//! Besides, all the fields have a getter and setter method.
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use named_tuple::named_tuple;
//! named_tuple!(
//!     #[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
//!     struct Human<'a> {
//!         name: &'a str,
//!         age: usize,
//!     }
//! );
//!
//! fn main() {
//!     let mut human = Human::new("alice", 18);
//!
//!     assert_eq!(human.field_names(), &["name", "age"]);
//!     assert_eq!(human.fields(), (("name", "alice"), ("age", 18)));
//!     assert_eq!(human.field_values(), ("alice", 18));
//!
//!     assert_eq!(human.name(), "alice");
//!     assert_eq!(human.age(), 18);
//!
//!     human.set_name("bob");
//!     human.set_age(20);
//!     assert_eq!(("bob", 20), human.into());
//! }
//! ```
//!
//! # Lifetimes
//!
//! A named tuple could have multi lifetimes.
//!
//! ```
//! # #[macro_use] extern crate named_tuple;
//! # use named_tuple::named_tuple;
//! pub struct Foo {}
//! pub enum Bar {}
//!
//! named_tuple!(
//!     pub struct Test<'a, 'b> {
//!         foo: &'a Foo,
//!         bar: &'b Bar,
//!     }
//! );
//! # fn main() {}
extern crate proc_macro;

use std::fmt;

use crate::proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    braced, parenthesized, parse_macro_input, parse_quote, token, Attribute, GenericParam,
    Generics, Ident, Lifetime, Meta, MetaList, NestedMeta, Path, Result, Token, Type, Visibility,
};

/// The macro used to generate tuple with named fields.
///
/// # Example
/// ```
/// # #[macro_use] extern crate named_tuple;
/// # use named_tuple::named_tuple;
/// named_tuple!(
///     #[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
///     struct Human<'a> {
///         name: &'a str,
///         age: usize,
///     }
/// );
///
/// fn main() {
///     let mut human = Human::new("alice", 18);
///
///     assert_eq!(human.field_names(), &["name", "age"]);
///     assert_eq!(human.fields(), (("name", "alice"), ("age", 18)));
///     assert_eq!(human.field_values(), ("alice", 18));
///
///     assert_eq!(human.name(), "alice");
///     assert_eq!(human.age(), 18);
///
///     assert_eq!(format!("{:?}", human), "Human { name: \"alice\", age: 18 }");
///
///     human.set_name("bob");
///     assert_eq!(human, ("bob", 18));
///
///     human.set_age(20);
///     assert_eq!(human, Human::from(("bob", 20)));
///
///     let t: (&str, usize) = human.into();
///
///     assert_eq!(t, ("bob", 20));
/// }
/// ```
///
/// If you don't care the field type, just use the tuple struct style.
///
/// ```
/// # #[macro_use] extern crate named_tuple;
/// # use named_tuple::named_tuple;
/// named_tuple!(
///     #[derive(Clone, Copy, Debug)]
///     struct Endpoint(host, port);
/// );
///
/// fn main() {
///     let mut endpoint = Endpoint::new("localhost", 80);
///
///     assert_eq!(endpoint.field_names(), &["host", "port"]);
///     assert_eq!(endpoint.fields(), (("host", "localhost"), ("port", 80)));
///     assert_eq!(endpoint.field_values(), ("localhost", 80));
///
///     assert_eq!(endpoint.host(), "localhost");
///     assert_eq!(endpoint.port(), 80);
///
///     assert_eq!(format!("{:?}", endpoint), "Endpoint { host: \"localhost\", port: 80 }");
///
///     endpoint.set_host("google.com");
///     endpoint.set_port(443);
///
///     assert_eq!(("google.com", 443), endpoint.into());
/// }
/// ```
#[proc_macro]
pub fn named_tuple(input: TokenStream) -> TokenStream {
    let NamedTuple {
        attrs,
        vis,
        _struct_token,
        name,
        mut generics,
        data,
    } = parse_macro_input!(input as NamedTuple);

    let derive = attrs
        .iter()
        .filter(|attr| attr.path.is_ident("derive"))
        .flat_map(|attr| attr.parse_meta())
        .flat_map(|meta| {
            if let Meta::List(MetaList { nested, .. }) = meta {
                nested
                    .iter()
                    .flat_map(|meta| {
                        if let NestedMeta::Meta(meta) = meta {
                            Some(meta.name())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            } else {
                vec![]
            }
            .into_iter()
        })
        .collect::<Vec<_>>();

    let as_ref = if derive.iter().any(|meta| meta.eq("Copy")) {
        None
    } else {
        Some(quote! { & })
    };

    let attrs = attrs
        .into_iter()
        .flat_map(|attr: Attribute| {
            if attr.path.is_ident("derive") {
                attr.parse_meta().map(|meta| {
                    if let Meta::List(MetaList {
                        ident,
                        paren_token,
                        nested,
                    }) = meta
                    {
                        let meta = Meta::List(MetaList {
                            ident,
                            paren_token,
                            nested: nested
                                .into_iter()
                                .filter(|meta| {
                                    if let NestedMeta::Meta(meta) = meta {
                                        meta.name() != "Debug"
                                            && meta.name() != "Serialize"
                                            && meta.name() != "Deserialize"
                                    } else {
                                        true
                                    }
                                })
                                .fold(Punctuated::new(), |mut nested, meta| {
                                    nested.push(meta);
                                    nested
                                }),
                        });

                        Attribute {
                            pound_token: attr.pound_token,
                            style: attr.style,
                            bracket_token: attr.bracket_token,
                            path: Path {
                                leading_colon: None,
                                segments: Punctuated::new(),
                            },
                            tts: quote!(#meta).into_token_stream(),
                        }
                    } else {
                        panic!("#[derive] attribute should contains Traits")
                    }
                })
            } else {
                Ok(attr)
            }
        })
        .collect::<Vec<_>>();

    let (need_type_bound, generics, name, fields) = match data {
        Data::Struct { fields, .. } => {
            let fields = fields
                .into_iter()
                .map(|field| (field.vis, field.name, field.ty))
                .collect::<Vec<_>>();

            (false, generics, name, fields)
        }
        Data::Tuple { fields, .. } => {
            generics.params.extend(
                (0..fields.len())
                    .map(|idx| Ident::new(&format!("T{}", idx), Span::call_site()))
                    .map(|ident| GenericParam::Type(ident.into())),
            );

            let fields = fields
                .into_iter()
                .enumerate()
                .map(|(idx, field)| {
                    let ty: Type = syn::parse_str(&format!("T{}", idx)).unwrap();

                    (field.vis, field.name, ty)
                })
                .collect::<Vec<_>>();

            (true, generics, name, fields)
        }
    };
    let append_type_bounds = |callback: fn(ty: &Type) -> TokenStream2| {
        let mut generics = generics.clone();

        let where_clause: syn::WhereClause = {
            let type_bounds = fields.iter().map(|(_, _, ty)| callback(ty));

            parse_quote! { where #(#type_bounds),* }
        };

        generics
            .make_where_clause()
            .predicates
            .extend(where_clause.predicates);

        generics
    };

    let tuple_type = {
        let types = fields.iter().map(|(_, _, ty)| ty);

        quote! { (#(#types),*) }
    };

    let field_accessors = fields
        .iter()
        .enumerate()
        .map(|(idx, (vis, name, field_ty))| {
            let getter = &name;
            let setter = Ident::new(&format!("set_{}", name), Span::call_site());

            quote! {
                #vis fn #getter(&self) -> #as_ref #field_ty {
                    #as_ref ((self.0).#idx)
                }
                #vis fn #setter(&mut self, v: #field_ty) {
                    (self.0).#idx = v;
                }
            }
        });

    let method_new = {
        let args = fields.iter().map(|(_, name, ty)| quote! { #name : #ty});
        let names = fields.iter().map(|(_, name, _)| name);

        quote! {
            pub fn new(#(#args),*) -> Self {
                #name((#(#names),*))
            }
        }
    };
    let method_field_names = {
        let field_names = fields.iter().map(|(_, name, _)| name.to_string());

        quote! {
            pub fn field_names(&self) -> &'static [&'static str] {
                &[#(#field_names),*]
            }
        }
    };
    let method_fields = {
        let field_types = fields.iter().map(|(_, _, ty)| {
            quote! { (&'static str, #as_ref #ty) }
        });
        let field_values = fields.iter().enumerate().map(|(idx, (_, name, _))| {
            let name = name.to_string();

            quote! { (#name, #as_ref ((self.0). #idx) ) }
        });

        quote! {
            pub fn fields(&self) -> (#(#field_types),*) {
                (#(#field_values),*)
            }
        }
    };
    let method_field_values = quote! {
        pub fn field_values(&self) -> #as_ref #tuple_type {
            #as_ref self.0
        }
    };

    let impl_tuple = {
        let generics = if as_ref.is_none() {
            append_type_bounds(|ty| quote! { #ty : Copy })
        } else {
            generics.clone()
        };

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        quote! {
            impl #impl_generics #name #ty_generics #where_clause {
                #(#field_accessors)*

                #method_new
                #method_field_names
                #method_fields
                #method_field_values
            }
        }
    };
    let impl_debug = if derive.iter().any(|meta| meta.eq("Debug")) {
        let generics = if need_type_bound {
            append_type_bounds(|ty| quote! { #ty : ::std::fmt::Debug })
        } else {
            generics.clone()
        };

        let struct_name = name.to_string();
        let fields = fields.iter().enumerate().map(|(idx, (_, name, _))| {
            let field_name = name.to_string();

            quote! {
                .field(#field_name, &(self.0).#idx)
            }
        });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause {
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    f.debug_struct(#struct_name) #(#fields)* .finish()
                }
            }
        })
    } else {
        None
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impl_from = quote! {
        impl #impl_generics From<#tuple_type> for #name #ty_generics #where_clause {
            fn from(t: #tuple_type) -> Self {
                #name(t)
            }
        }
    };
    let impl_into = quote! {
        impl #impl_generics Into<#tuple_type> for #name #ty_generics #where_clause {
            fn into(self) -> #tuple_type {
                self.0
            }
        }
    };
    let impl_deref = quote! {
        impl #impl_generics ::std::ops::Deref for #name #ty_generics #where_clause {
            type Target = #tuple_type;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
    let impl_deref_mut = quote! {
        impl #impl_generics ::std::ops::DerefMut for #name #ty_generics #where_clause {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
    let impl_partial_eq = if derive.iter().any(|meta| meta.eq("PartialEq")) {
        Some(quote! {
            impl #impl_generics ::std::cmp::PartialEq<#tuple_type> for #name #ty_generics #where_clause {
                fn eq(&self, other: & #tuple_type) -> bool {
                    (self.0).eq(other)
                }
            }
        })
    } else if need_type_bound {
        let generics = append_type_bounds(|ty| quote! { #ty : ::std::cmp::PartialEq });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics ::std::cmp::PartialEq for #name #ty_generics #where_clause {
                fn eq(&self, other: &Self) -> bool {
                    (self.0).eq(&other.0)
                }
            }

            impl #impl_generics ::std::cmp::PartialEq<#tuple_type> for #name #ty_generics #where_clause {
                fn eq(&self, other: & #tuple_type) -> bool {
                    (self.0).eq(other)
                }
            }
        })
    } else {
        None
    };
    let impl_partial_ord = if derive.iter().any(|meta| meta.eq("PartialOrd")) {
        Some(quote! {
            impl #impl_generics ::std::cmp::PartialOrd<#tuple_type> for #name #ty_generics #where_clause {
                fn partial_cmp(&self, other: & #tuple_type) -> Option<::std::cmp::Ordering> {
                    (self.0).partial_cmp(other)
                }
            }
        })
    } else if need_type_bound {
        let generics = append_type_bounds(|ty| quote! { #ty : ::std::cmp::PartialOrd });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics ::std::cmp::PartialOrd for #name #ty_generics #where_clause {
                fn partial_cmp(&self, other: &Self) -> Option<::std::cmp::Ordering> {
                    (self.0).partial_cmp(&other.0)
                }
            }

            impl #impl_generics ::std::cmp::PartialOrd<#tuple_type> for #name #ty_generics #where_clause {
                fn partial_cmp(&self, other: & #tuple_type) -> Option<::std::cmp::Ordering> {
                    (self.0).partial_cmp(other)
                }
            }
        })
    } else {
        None
    };
    let impl_clone = if need_type_bound && !derive.iter().any(|meta| meta.eq("Clone")) {
        let generics = append_type_bounds(|ty| quote! { #ty : Clone });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics Clone for #name #ty_generics #where_clause {
                fn clone(&self) -> Self {
                    #name(self.0.clone())
                }
            }
        })
    } else {
        None
    };
    let impl_copy = if need_type_bound && !derive.iter().any(|meta| meta.eq("Copy")) {
        let generics = append_type_bounds(|ty| quote! { #ty : Copy });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics Copy for #name  #ty_generics #where_clause {}
        })
    } else {
        None
    };
    let impl_default = if need_type_bound && !derive.iter().any(|meta| meta.eq("Default")) {
        let generics = append_type_bounds(|ty| quote! { #ty : Default });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics Default for #name  #ty_generics #where_clause {
                fn default() -> Self {
                    #name(Default::default())
                }
            }
        })
    } else {
        None
    };
    let impl_hash = if need_type_bound && !derive.iter().any(|meta| meta.eq("Hash")) {
        let generics = append_type_bounds(|ty| quote! { #ty : ::std::hash::Hash });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics ::std::hash::Hash for #name #ty_generics #where_clause {
                fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
                    self.0.hash(state)
                }
            }
        })
    } else {
        None
    };
    let impl_serialize = if cfg!(feature = "serde")
        && (need_type_bound || derive.iter().any(|meta| meta.eq("Serialize")))
    {
        let generics = if need_type_bound {
            append_type_bounds(|ty| quote! { #ty : ::serde::Serialize })
        } else {
            generics.clone()
        };

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics ::serde::Serialize for #name #ty_generics #where_clause {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: ::serde::Serializer,
                {
                    self.0.serialize(serializer)
                }
            }
        })
    } else {
        None
    };
    let impl_deserialize = if cfg!(feature = "serde")
        && (need_type_bound || derive.iter().any(|meta| meta.eq("Deserialize")))
    {
        let generics = if need_type_bound {
            append_type_bounds(|ty| quote! { #ty : ::serde::Deserialize<'de> })
        } else {
            generics.clone()
        };

        let (_, ty_generics, _) = generics.split_for_impl();

        let mut generics = generics.clone();

        let bounds = generics
            .params
            .iter()
            .flat_map(|param| match param {
                syn::GenericParam::Lifetime(syn::LifetimeDef { lifetime, .. })
                    if lifetime.ident != "de" =>
                {
                    Some(lifetime.clone())
                }
                _ => None,
            })
            .collect();
        generics.params.insert(
            0,
            syn::LifetimeDef {
                attrs: vec![],
                lifetime: Lifetime::new("'de", Span::call_site()),
                colon_token: None,
                bounds,
            }
            .into(),
        );

        let (impl_generics, _, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics ::serde::Deserialize<'de> for #name #ty_generics #where_clause {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: ::serde::Deserializer<'de>
                {
                    < #tuple_type >::deserialize(deserializer).map(#name)
                }
            }
        })
    } else {
        None
    };

    let expanded = quote! {
        #[repr(transparent)]
        #(#attrs)*
        #vis struct #name #ty_generics (#tuple_type);

        #impl_tuple
        #impl_debug
        #impl_from
        #impl_into
        #impl_deref
        #impl_deref_mut
        #impl_partial_eq
        #impl_partial_ord
        #impl_clone
        #impl_copy
        #impl_default
        #impl_hash
        #impl_serialize
        #impl_deserialize
    };

    // eprintln!("{}", expanded.to_string());

    TokenStream::from(expanded)
}

struct NamedTuple {
    attrs: Vec<Attribute>,
    vis: Visibility,
    _struct_token: Token![struct],
    name: Ident,
    generics: Generics,
    data: Data,
}

enum Data {
    Struct {
        _brace_token: token::Brace,
        fields: Punctuated<StructField, Token![,]>,
    },
    Tuple {
        _paren_token: token::Paren,
        fields: Punctuated<TupleField, Token![,]>,
        _semi_tokne: Token![;],
    },
}

struct Lifetimes {
    _lt_token: Token![<],
    lifetimes: Punctuated<Lifetime, Token![,]>,
    _gt_token: Token![>],
}

struct StructField {
    vis: Visibility,
    name: Ident,
    colon_token: Token![:],
    ty: Type,
}

struct TupleField {
    vis: Visibility,
    name: Ident,
}

impl fmt::Debug for NamedTuple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("NamedTuple")
            .field(
                "attributes",
                &self
                    .attrs
                    .iter()
                    .map(|attr| {
                        let expanded = quote! { #attr };

                        expanded.to_string()
                    })
                    .collect::<Vec<_>>(),
            )
            .field("visibility", {
                let vis = &self.vis;

                let expanded = quote! { #vis };

                &expanded.to_string()
            })
            .field("name", &self.name.to_string())
            .field("data", &self.data)
            .finish()
    }
}

impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Data::Struct { fields, .. } => f
                .debug_struct("Struct")
                .field("fields", &fields.iter().collect::<Vec<_>>())
                .finish(),
            Data::Tuple { fields, .. } => {
                let mut t = f.debug_tuple("Tuple");

                for field in fields {
                    t.field(&field);
                }

                t.finish()
            }
        }
    }
}

impl fmt::Debug for StructField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("StructField")
            .field("visibility", {
                let vis = &self.vis;

                let expanded = quote! { #vis };

                &expanded.to_string()
            })
            .field("name", &self.name)
            .field("ty", {
                let ty = &self.ty;

                let expanded = quote! { #ty };

                &expanded.to_string()
            })
            .finish()
    }
}

impl fmt::Debug for TupleField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("TupleField")
            .field("visibility", {
                let vis = &self.vis;

                let expanded = quote! { #vis };

                &expanded.to_string()
            })
            .field("name", &self.name)
            .finish()
    }
}

impl Parse for NamedTuple {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(NamedTuple {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            _struct_token: input.parse()?,
            name: input.parse()?,
            generics: if input.peek(Token![<]) {
                input.parse()?
            } else {
                Default::default()
            },
            data: input.parse()?,
        })
    }
}

impl Parse for Data {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(token::Brace) {
            let content;

            Ok(Data::Struct {
                _brace_token: braced!(content in input),
                fields: content.parse_terminated(StructField::parse)?,
            })
        } else if lookahead.peek(token::Paren) {
            let content;

            Ok(Data::Tuple {
                _paren_token: parenthesized!(content in input),
                fields: content.parse_terminated(TupleField::parse)?,
                _semi_tokne: input.parse()?,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Lifetimes {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Lifetimes {
            _lt_token: input.parse()?,
            lifetimes: {
                let mut lifetimes = Punctuated::new();
                while !input.peek(Token![>]) {
                    lifetimes.push_value(input.parse()?);
                    if input.peek(Token![>]) {
                        break;
                    }
                    lifetimes.push_punct(input.parse()?);
                }
                lifetimes
            },
            _gt_token: input.parse()?,
        })
    }
}

impl Parse for StructField {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(StructField {
            vis: input.parse()?,
            name: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl Parse for TupleField {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(TupleField {
            vis: input.parse()?,
            name: input.parse()?,
        })
    }
}

impl ToTokens for StructField {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let StructField {
            vis: _,
            name,
            colon_token,
            ty,
        } = self;

        let expanded = quote! {
            #name #colon_token #ty
        };

        expanded.to_tokens(tokens)
    }
}

impl ToTokens for TupleField {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TupleField { vis: _, name } = self;

        name.to_tokens(tokens)
    }
}

impl ToTokens for Lifetimes {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let lifetimes = self.lifetimes.iter();

        let expanded = quote! {
            < #(#lifetimes),* >
        };

        expanded.to_tokens(tokens)
    }
}
