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
    Generics, Ident, Lifetime, Lit, Meta, MetaList, NestedMeta, Path, Result, Token, Type,
    Visibility,
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

    let has_derive = |name: &str| derive.iter().any(|meta| meta.eq(name));

    let as_ref = if has_derive("Copy") {
        None
    } else {
        Some(quote! { & })
    };

    let mut filter = DeriveFilter(|meta| {
        if let NestedMeta::Meta(meta) = meta {
            meta.name() == "Debug"
                || (cfg!(feature = "serde")
                    && (meta.name() == "Serialize" || meta.name() == "Deserialize"))
        } else {
            false
        }
    });
    let attrs = attrs.into_iter().map(|attr| {
        if attr.path.is_ident("derive") {
            filter.fold_attribute(attr)
        } else {
            attr
        }
    });

    let mut has_default_value = false;
    let mut has_type_bound = false;
    let fields = match data {
        Data::Struct { fields, .. } | Data::Tuple { fields, .. } => fields,
    }
    .into_iter()
    .enumerate()
    .map(|(idx, field)| {
        (
            field.vis,
            field.name,
            field.ty.map_or_else(
                || {
                    let type_name = Ident::new(&format!("T{}", idx), Span::call_site());
                    let type_path = syn::Type::Path(syn::parse_quote! { #type_name });

                    generics.params.push(GenericParam::Type(type_name.into()));

                    has_type_bound = true;

                    type_path
                },
                |(_, ty)| ty,
            ),
            field.default.map(|(_, value)| {
                has_default_value = true;

                value
            }),
        )
    })
    .collect::<Vec<_>>();

    let append_type_bounds = |callback: fn(ty: &Type) -> TokenStream2| {
        let mut generics = generics.clone();

        let where_clause: syn::WhereClause = {
            let type_bounds = fields.iter().map(|(_, _, ty, _)| callback(ty));

            parse_quote! { where #(#type_bounds),* }
        };

        generics
            .make_where_clause()
            .predicates
            .extend(where_clause.predicates);

        generics
    };

    let tuple_type = {
        let types = fields.iter().map(|(_, _, ty, _)| ty);

        quote! { (#(#types),*) }
    };

    let field_accessors = fields
        .iter()
        .enumerate()
        .map(|(idx, (vis, name, field_ty, _))| {
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
        let args = fields.iter().map(|(_, name, ty, _)| quote! { #name : #ty});
        let names = fields.iter().map(|(_, name, _, _)| name);

        quote! {
            pub fn new(#(#args),*) -> Self {
                #name((#(#names),*))
            }
        }
    };
    let method_field_names = {
        let field_names = fields.iter().map(|(_, name, _, _)| name.to_string());

        quote! {
            pub fn field_names(&self) -> &'static [&'static str] {
                &[#(#field_names),*]
            }
        }
    };
    let method_fields = {
        let field_types = fields.iter().map(|(_, _, ty, _)| {
            quote! { (&'static str, #as_ref #ty) }
        });
        let field_values = fields.iter().enumerate().map(|(idx, (_, name, _, _))| {
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

    let impl_debug = if has_derive("Debug") {
        let generics = if has_type_bound {
            append_type_bounds(|ty| quote! { #ty : ::std::fmt::Debug })
        } else {
            generics.clone()
        };

        let struct_name = name.to_string();
        let fields = fields.iter().enumerate().map(|(idx, (_, name, _, _))| {
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
    let impl_partial_eq = if has_derive("PartialEq") {
        Some(quote! {
            impl #impl_generics ::std::cmp::PartialEq<#tuple_type> for #name #ty_generics #where_clause {
                fn eq(&self, other: & #tuple_type) -> bool {
                    (self.0).eq(other)
                }
            }
        })
    } else if has_type_bound {
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
    let impl_partial_ord = if has_derive("PartialOrd") {
        Some(quote! {
            impl #impl_generics ::std::cmp::PartialOrd<#tuple_type> for #name #ty_generics #where_clause {
                fn partial_cmp(&self, other: & #tuple_type) -> Option<::std::cmp::Ordering> {
                    (self.0).partial_cmp(other)
                }
            }
        })
    } else if has_type_bound {
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
    let impl_clone = if has_type_bound && !has_derive("Clone") {
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
    let impl_copy = if has_type_bound && !has_derive("Copy") {
        let generics = append_type_bounds(|ty| quote! { #ty : Copy });

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        Some(quote! {
            impl #impl_generics Copy for #name  #ty_generics #where_clause {}
        })
    } else {
        None
    };
    let impl_default = if has_type_bound && !has_derive("Default") {
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
    let impl_hash = if has_type_bound && !has_derive("Hash") {
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
    let impl_serialize = if cfg!(feature = "serde") && (has_type_bound || has_derive("Serialize")) {
        let generics = if has_type_bound {
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
    let impl_deserialize =
        if cfg!(feature = "serde") && (has_type_bound || has_derive("Deserialize")) {
            let generics = if has_type_bound {
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

    if cfg!(feature = "dump") {
        dump_code(expanded.to_string())
    }

    TokenStream::from(expanded)
}

#[cfg(not(feature = "dump"))]
fn dump_code(_: String) {}

#[cfg(feature = "dump")]
fn dump_code(expanded: String) {
    use std::io::{stdout, Write};

    use rustfmt_nightly::*;

    let mut config = Config::default();
    config.set().emit_mode(EmitMode::Stdout);
    let stderr = stdout();
    let mut handle = stderr.lock();
    let mut session = Session::new(config, Some(&mut handle));
    let input = Input::Text(expanded);
    let report = session.format(input).unwrap();
    if report.has_warnings() && session.config.color().use_colored_tty() {
        report.fancy_print(::term::stderr().unwrap()).unwrap();
    }
    stdout().flush().unwrap();
}

struct DeriveFilter(fn(&syn::NestedMeta) -> bool);

impl DeriveFilter {
    fn fold_attribute(&mut self, attr: syn::Attribute) -> syn::Attribute {
        let meta = syn::fold::fold_meta(self, attr.parse_meta().expect("meta"));

        syn::Attribute {
            pound_token: attr.pound_token,
            style: attr.style,
            bracket_token: attr.bracket_token,
            path: Path {
                leading_colon: None,
                segments: Punctuated::new(),
            },
            tts: quote!(#meta).into_token_stream(),
        }
    }
}

impl syn::fold::Fold for DeriveFilter {
    fn fold_meta_list(&mut self, meta_list: MetaList) -> MetaList {
        syn::MetaList {
            ident: meta_list.ident,
            paren_token: meta_list.paren_token,
            nested: meta_list
                .nested
                .into_iter()
                .filter(|meta| !self.0(meta))
                .collect(),
        }
    }
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
        fields: Punctuated<Field, Token![,]>,
    },
    Tuple {
        _paren_token: token::Paren,
        fields: Punctuated<Field, Token![,]>,
        _semi_tokne: Token![;],
    },
}

struct Field {
    vis: Visibility,
    name: Ident,
    ty: Option<(Token![:], Type)>,
    default: Option<(Token![=], Lit)>,
}

impl fmt::Debug for NamedTuple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("NamedTuple")
            .field(
                "attributes",
                &self
                    .attrs
                    .iter()
                    .map(|attr| quote!(#attr).to_string())
                    .collect::<Vec<_>>(),
            )
            .field("visibility", {
                let vis = &self.vis;

                &quote!(#vis).to_string()
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

impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = f.debug_struct("Field");

        s.field("visibility", {
            let vis = &self.vis;

            &quote!(#vis).to_string()
        })
        .field("name", &self.name);

        if let Some((_, ref ty)) = self.ty {
            s.field("type", { &quote!(#ty).to_string() });
        }

        if let Some((_, ref value)) = self.default {
            s.field("default", { &quote!(#value).to_string() });
        }

        s.finish()
    }
}

impl Parse for NamedTuple {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(NamedTuple {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            _struct_token: input.parse()?,
            name: input.parse()?,
            generics: input.parse()?,
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
                fields: content.parse_terminated(Field::parse)?,
            })
        } else if lookahead.peek(token::Paren) {
            let content;

            Ok(Data::Tuple {
                _paren_token: parenthesized!(content in input),
                fields: content.parse_terminated(Field::parse)?,
                _semi_tokne: input.parse()?,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Field {
            vis: input.parse()?,
            name: input.parse()?,
            ty: if input.peek(Token![:]) {
                Some((input.parse()?, input.parse()?))
            } else {
                None
            },
            default: if input.peek(Token![=]) {
                Some((input.parse()?, input.parse()?))
            } else {
                None
            },
        })
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Field { name, ty, .. } = self;

        if let Some((_, ty)) = ty {
            quote! { #name : #ty }
        } else {
            quote! { #name }
        }
        .to_tokens(tokens)
    }
}
