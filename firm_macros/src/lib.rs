use std::any::{Any, TypeId};

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::spanned::Spanned;
use syn::{ItemStruct, Type, TypePath, TypeTuple};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
};

struct ParsedAstNode(ItemStruct);

impl Parse for ParsedAstNode {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse().map(Self)
    }
}

impl ToTokens for ParsedAstNode {
    fn to_tokens(&self, tokens: &mut TokenStream2) -> () {
        let Self(ast_node) = self;

        let name = ast_node.ident.clone();
        let impls = ast_node.fields.iter().filter_map(|field| {
            let mut related = false;
            let mut related_maybe = false;
            let mut related_many = false;
            let mut related_many_maybe = false;

            for attr in &field.attrs {
                if attr.path().is_ident("related") {
                    related = true;
                } else if attr.path().is_ident("related_maybe") {
                    related_maybe = true;
                } else if attr.path().is_ident("related_many") {
                    related_many = true;
                } else if attr.path().is_ident("related_many_maybe") {
                    related_many_maybe = true;
                }
            }

            let true_num = (related as usize)
                + (related_maybe as usize)
                + (related_many as usize)
                + (related_many_maybe as usize);

            assert!(true_num <= 1, "Only one of related, related_maybe, related_many, or related_many_maybe can be used on a field");

            if true_num == 0 {
                return None;
            }

            let name = field.ident.clone().unwrap();
            return Some(
                if related {
                    quote! {
                        pub fn #name<'a>(&self, nodes: &'a [Stmt]) -> &'a Stmt {
                            &nodes[self.#name]
                        }
                    }
                } else if related_maybe {
                    quote! {
                        pub fn #name<'a>(&self, nodes: &'a [Stmt]) -> Option<&'a Stmt> {
                            self.#name.map(|index| &nodes[index])
                        }
                    }
                } else if related_many {
                    quote! {
                        pub fn #name<'a>(&self, nodes: &'a [Stmt]) -> &'a [Stmt] {
                            &nodes[self.#name.0..self.#name.0 + self.#name.1]
                        }
                    }
                } else if related_many_maybe {
                    quote! {
                        pub fn #name<'a>(&self, nodes: &'a [Stmt]) -> Option<&'a [Stmt]> {
                            self.#name.map(|(start, len)| &nodes[start..start + len])
                        }
                    }
                } else {
                    unreachable!()
                },
            );
        });

        let output = {
            quote! {
                impl #name {
                    #(#impls)*
                }
            }
        };

        tokens.extend(output);
    }
}

#[proc_macro_derive(
    AstNode,
    attributes(related, related_maybe, related_many, related_many_maybe)
)]
pub fn ast_node(item: TokenStream) -> TokenStream {
    let node = parse_macro_input!(item as ParsedAstNode);
    quote! { #node }.into()
}
