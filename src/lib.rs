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
//!     struct Human<'a> {
//!         name: &'a str,
//!         age: usize,
//!     }
//! );
//!
//! fn main() {
//!     let human: Human = ("alice", 18).into();
//!
//!     assert_eq!(human.name(), "alice");
//!     assert_eq!(human.age(), 18);
//!     assert_eq!(human.field_values(), ("alice", 18));
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
//! //! # #[macro_use] extern crate named_tuple;
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
//!     assert_eq!(Human::field_names(), &["name", "age"]);
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
    braced, parse_macro_input, token, Attribute, Ident, Lifetime, Meta, MetaList, NestedMeta, Path,
    Result, Token, Type, Visibility,
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
///     assert_eq!(Human::field_names(), &["name", "age"]);
///     assert_eq!(human.fields(), (("name", "alice"), ("age", 18)));
///     assert_eq!(human.field_values(), ("alice", 18));
///
///     assert_eq!(human.name(), "alice");
///     assert_eq!(human.age(), 18);
///
///     assert_eq!((human.0).0, "alice");
///     assert_eq!((human.0).1, 18);
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
#[proc_macro]
pub fn named_tuple(input: TokenStream) -> TokenStream {
    let NamedTuple {
        attrs,
        vis,
        _struct_token,
        name,
        lifetimes,
        _brace_token,
        fields,
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

    let tuple_type = {
        let field_types = fields.iter().map(|field| &field.ty);

        quote! { (#(#field_types),*) }
    };
    let field_accessors = fields.iter().enumerate().map(|(idx, field)| {
        let vis = &field.vis;
        let field_ty = &field.ty;
        let getter = &field.name;
        let setter = Ident::new(&format!("set_{}", field.name), Span::call_site());

        quote! {
            #vis fn #getter(&self) -> #field_ty {
                (self.0).#idx
            }
            #vis fn #setter(&mut self, v: #field_ty) {
                (self.0).#idx = v;
            }
        }
    });
    let method_new = {
        let fields = &fields;
        let field_names = fields.iter().map(|field| &field.name);

        quote! {
            pub fn new(#(#fields),*) -> #name #lifetimes {
                #name((#(#field_names),*))
            }
        }
    };
    let method_field_names = {
        let field_names = fields.iter().map(|field| field.name.to_string());

        quote! {
            pub fn field_names() -> &'static [&'static str] {
                &[#(#field_names),*]
            }
        }
    };
    let method_fields = {
        let field_types = fields.iter().map(|field| {
            let ty = &field.ty;

            quote! { (&'static str, #ty) }
        });
        let field_values = fields.iter().enumerate().map(|(idx, field)| {
            let name = field.name.to_string();

            quote! { (#name, (self.0). #idx ) }
        });

        quote! {
            pub fn fields(&self) -> (#(#field_types),*) {
                (#(#field_values),*)
            }
        }
    };
    let method_field_values = quote! {
        pub fn field_values(&self) -> #tuple_type {
            self.0
        }
    };
    let impl_debug = if derive.iter().any(|meta| meta.eq("Debug")) {
        let fields = fields.iter().enumerate().map(|(idx, field)| {
            let field_name = field.name.to_string();

            quote! {
                .field(#field_name, &(self.0).#idx)
            }
        });

        let struct_name = name.to_string();

        Some(quote! {
            impl #lifetimes ::std::fmt::Debug for #name #lifetimes {
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    f.debug_struct(#struct_name) #(#fields)* .finish()
                }
            }
        })
    } else {
        None
    };
    let impl_from = quote! {
        impl #lifetimes From<#tuple_type> for #name #lifetimes {
            fn from(t: #tuple_type) -> Self {
                #name(t)
            }
        }
    };
    let impl_into = quote! {
        impl #lifetimes Into<#tuple_type> for #name #lifetimes {
            fn into(self) -> #tuple_type {
                self.0
            }
        }
    };
    let impl_deref = quote! {
        impl #lifetimes ::std::ops::Deref for #name #lifetimes {
            type Target = #tuple_type;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
    let impl_deref_mut = quote! {
        impl #lifetimes ::std::ops::DerefMut for #name #lifetimes {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
    let impl_partial_eq = if derive.iter().any(|meta| meta.eq("PartialEq")) {
        Some(quote! {
            impl #lifetimes ::std::cmp::PartialEq<#tuple_type> for #name #lifetimes {
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
            impl #lifetimes ::std::cmp::PartialOrd<#tuple_type> for #name #lifetimes {
                fn partial_cmp(&self, other: & #tuple_type) -> Option<::std::cmp::Ordering> {
                    (self.0).partial_cmp(other)
                }
            }
        })
    } else {
        None
    };

    let expanded = quote! {
        #[repr(transparent)]
        #(#attrs)*
        #vis struct #name #lifetimes (#tuple_type);

        impl #lifetimes #name #lifetimes{
            #(#field_accessors)*

            #method_new
            #method_field_names
            #method_fields
            #method_field_values
        }

        #impl_debug
        #impl_from
        #impl_into
        #impl_deref
        #impl_deref_mut
        #impl_partial_eq
        #impl_partial_ord
    };

    //eprintln!("{}", expanded.to_string());

    TokenStream::from(expanded)
}

struct NamedTuple {
    attrs: Vec<Attribute>,
    vis: Visibility,
    _struct_token: Token![struct],
    name: Ident,
    lifetimes: Option<Lifetimes>,
    _brace_token: token::Brace,
    fields: Punctuated<Field, Token![,]>,
}

struct Lifetimes {
    _lt_token: Token![<],
    lifetimes: Punctuated<Lifetime, Token![,]>,
    _gt_token: Token![>],
}

struct Field {
    vis: Visibility,
    name: Ident,
    colon_token: Token![:],
    ty: Type,
}

impl fmt::Debug for NamedTuple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("NamedTuple")
            .field("visibility", {
                let vis = &self.vis;

                let expanded = quote! { #vis };

                &expanded.to_string()
            })
            .field("name", &self.name)
            .field("fields", &self.fields.iter().collect::<Vec<_>>())
            .finish()
    }
}

impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Field")
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

impl Parse for NamedTuple {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;

        Ok(NamedTuple {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            _struct_token: input.parse()?,
            name: input.parse()?,
            lifetimes: if input.peek(Token![<]) {
                Some(input.parse()?)
            } else {
                None
            },
            _brace_token: braced!(content in input),
            fields: content.parse_terminated(Field::parse)?,
        })
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

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Field {
            vis: input.parse()?,
            name: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Field {
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

impl ToTokens for Lifetimes {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let lifetimes = self.lifetimes.iter();

        let expanded = quote! {
            < #(#lifetimes),* >
        };

        expanded.to_tokens(tokens)
    }
}
