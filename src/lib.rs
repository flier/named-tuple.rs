extern crate proc_macro;

use std::fmt;

use crate::proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    braced, parse_macro_input, token, Attribute, Ident, Lifetime, Meta, MetaList, NestedMeta, Path,
    Result, Token, Type, Visibility,
};

#[proc_macro]
pub fn named_tuple(input: TokenStream) -> TokenStream {
    let NamedTuple {
        attrs,
        vis,
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

    let field_names = fields.iter().map(|field| &field.name);
    let field_types = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();
    let field_types = &field_types;
    let tuple_type = quote! { (#(#field_types),*) };

    let field_accessors = fields.iter().enumerate().map(|(idx, field)| {
        let field_name = &field.name;
        let field_ty = &field.ty;

        quote! {
            pub fn #field_name(&self) -> #field_ty {
                (self.0).#idx
            }
        }
    });
    let fn_new = {
        let args = fields.iter().map(|field| {
            let field_name = &field.name;
            let field_ty = &field.ty;

            quote! {
                #field_name : #field_ty
            }
        });

        quote! {
            pub fn new(#(#args),*) -> #name #lifetimes {
                #name((#(#field_names),*))
            }
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

            #fn_new
        }

        #impl_debug
        #impl_from
        #impl_into
        #impl_deref
        #impl_deref_mut
        #impl_partial_eq
        #impl_partial_ord
    };

    TokenStream::from(expanded)
}

struct NamedTuple {
    attrs: Vec<Attribute>,
    vis: Visibility,
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
    name: Ident,
    _colon_token: Token![:],
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
            name: input.parse()?,
            _colon_token: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl ToTokens for Lifetimes {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let lifetimes = self.lifetimes.iter();

        let expanded = quote! {
            < #(#lifetimes),* >
        };

        expanded.to_tokens(tokens)
    }
}
