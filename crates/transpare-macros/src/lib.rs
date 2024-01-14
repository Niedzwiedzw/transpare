use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma, DataEnum,
    DataStruct, DeriveInput, Field, FieldsNamed, FieldsUnnamed, GenericParam, Generics, Ident,
    Index, Type, TypeParam, TypePath, Variant,
};
use tap::prelude::*;

#[derive(Clone)]
enum AnyFieldIdent {
    Name(Ident),
    Index(Index),
}

impl quote::ToTokens for AnyFieldIdent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            AnyFieldIdent::Name(name) => name.to_tokens(tokens),
            AnyFieldIdent::Index(index) => index.to_tokens(tokens),
        }
    }
}

fn field_types<'a>(fields: impl Iterator<Item = &'a Field>) -> Vec<(AnyFieldIdent, Ident)> {
    fields
        .enumerate()
        .filter_map(|(idx, field)| {
            type_ident(field).expect("no type").pipe(Some).map(|ty| {
                field
                    .ident
                    .clone()
                    .map(AnyFieldIdent::from)
                    .unwrap_or_else(|| Index::from(idx).pipe(AnyFieldIdent::from))
                    .pipe(|ident| (ident, ty))
            })
        })
        .collect()
}

impl From<Ident> for AnyFieldIdent {
    fn from(value: Ident) -> Self {
        value.pipe(Self::Name)
    }
}

impl From<Index> for AnyFieldIdent {
    fn from(value: Index) -> Self {
        value.pipe(Self::Index)
    }
}

fn map_params<F: FnMut(GenericParam) -> GenericParam>(
    punctuated: Punctuated<GenericParam, Comma>,
    map_params: F,
) -> Punctuated<GenericParam, Comma> {
    punctuated.into_iter().map(map_params).collect()
}

fn replace_param_with(
    punctuated: Punctuated<GenericParam, Comma>,
    replaced: &Ident,
    to: &Ident,
) -> Punctuated<GenericParam, Comma> {
    map_params(punctuated, |param| match param {
        GenericParam::Type(ty) => {
            let ident = replaced
                .eq(&ty.ident)
                .then_some(())
                .map(|_| to.clone())
                .unwrap_or_else(|| ty.ident.clone());
            GenericParam::Type(TypeParam { ident, ..ty })
        }
        other => other,
    })
}

#[proc_macro_derive(Transpare, attributes(transpare))]
pub fn derive_transpare(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        vis,
        ident: struct_name,
        generics: Generics {
            params: generic_params,
            ..
        },
        data,
        ..
    } = parse_macro_input!(input as DeriveInput);
    let impls = generic_params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(TypeParam { ident, .. }) => Some(ident),
            _ => None,
        })
        .flat_map(|generic_parameter| {
            let generic_params = generic_params.clone();
            let struct_name = struct_name.clone();
            let target_parameter = Ident::new("__U", generic_params.span());
            let err_parameter = Ident::new("__E", generic_params.span());

            match &data {
                syn::Data::Struct(DataStruct { fields, .. }) => match fields {
                    syn::Fields::Named(FieldsNamed { named, .. }) => {
                        let method_name = |prefix: &str| {
                            Ident::new(
                                format!(
                                    "{prefix}_{}",
                                    heck::AsSnakeCase(generic_parameter.to_string())
                                )
                                .as_str(),
                                generic_parameter.span(),
                            )
                        };

                        let map = {
                            let method_name = method_name("map");
                            let target_parameters = replace_param_with(
                                generic_params.clone(),
                                generic_parameter,
                                &target_parameter,
                            );
                            let fields =
                                field_types(named.iter()).into_iter().map(|(field, ty)| {
                                    match &ty == generic_parameter {
                                        false => quote! {
                                            #field: self.#field
                                        },
                                        true => quote! {
                                            #field: #method_name(self.#field)
                                        },
                                    }
                                });
                            quote! {
                                #vis fn #method_name<
                                    #target_parameter,
                                    F: FnMut(#generic_parameter) -> #target_parameter,
                               >(
                                    self,
                                    mut #method_name: F,
                                ) -> #struct_name<#target_parameters> {

                                    #struct_name {
                                        #(#fields,)*
                                    }
                                }
                            }
                        };
                        let try_map = {
                            let method_name = method_name("try_map");
                            let target_parameters = replace_param_with(
                                generic_params.clone(),
                                generic_parameter,
                                &target_parameter,
                            );
                            let fields =
                                field_types(named.iter()).into_iter().map(|(field, ty)| {
                                    match &ty == generic_parameter {
                                        false => quote! {
                                            #field: self.#field
                                        },
                                        true => quote! {
                                            #field: #method_name(self.#field)?
                                        },
                                    }
                                });
                            quote! {
                                #vis fn #method_name<
                                    #err_parameter,
                                    #target_parameter,
                                    F: FnMut(#generic_parameter) -> std::result::Result<
                                        #target_parameter,
                                        #err_parameter,
                                    >,
                               >(
                                    self,
                                    mut #method_name: F,
                                ) -> std::result::Result<
                                        #struct_name<
                                            #target_parameters>,
                                            #err_parameter,
                                        > {
                                    Ok(#struct_name {
                                        #(#fields,)*
                                    })
                                }
                            }
                        };
                        quote! {
                            #[automatically_derived]
                            impl <#generic_params> #struct_name<#generic_params> {
                                #map
                                #try_map
                            }
                        }
                    }
                    syn::Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                        let method_name = |prefix: &str| {
                            Ident::new(
                                format!(
                                    "{prefix}_{}",
                                    heck::AsSnakeCase(generic_parameter.to_string())
                                )
                                .as_str(),
                                generic_parameter.span(),
                            )
                        };

                        let map = {
                            let method_name = method_name("map");
                            let target_parameters = replace_param_with(
                                generic_params.clone(),
                                generic_parameter,
                                &target_parameter,
                            );
                            let fields = field_types(unnamed.iter()).into_iter().map(
                                |(field, ty)| match &ty == generic_parameter {
                                    false => quote! {
                                        self.#field
                                    },
                                    true => quote! {
                                        #method_name(self.#field)
                                    },
                                },
                            );
                            quote! {
                                #vis fn #method_name<
                                    #target_parameter,
                                    F: FnMut(#generic_parameter) -> #target_parameter,
                               >(
                                    self,
                                    mut #method_name: F,
                                ) -> #struct_name<#target_parameters> {

                                    #struct_name (
                                        #(#fields,)*
                                    )
                                }
                            }
                        };
                        let try_map = {
                            let method_name = method_name("try_map");
                            let target_parameters = replace_param_with(
                                generic_params.clone(),
                                generic_parameter,
                                &target_parameter,
                            );
                            let fields = field_types(unnamed.iter()).into_iter().map(
                                |(field, ty)| match &ty == generic_parameter {
                                    false => quote! {
                                        self.#field
                                    },
                                    true => quote! {
                                        #method_name(self.#field)?
                                    },
                                },
                            );
                            quote! {
                                #vis fn #method_name<
                                    #err_parameter,
                                    #target_parameter,
                                    F: FnMut(#generic_parameter) -> std::result::Result<
                                        #target_parameter,
                                        #err_parameter,
                                    >,
                               >(
                                    self,
                                    mut #method_name: F,
                                ) -> std::result::Result<
                                        #struct_name<#target_parameters>,
                                        #err_parameter,
                                    > {
                                    Ok(#struct_name (
                                        #(#fields,)*
                                    ))
                                }
                            }
                        };
                        quote! {
                            #[automatically_derived]
                            impl <#generic_params> #struct_name<#generic_params> {
                                #map
                                #try_map
                            }
                        }
                    }
                    syn::Fields::Unit => {
                        quote! {}
                    }
                },
                syn::Data::Enum(DataEnum { variants, .. }) => {
                    let method_name = |prefix: &str| {
                        Ident::new(
                            format!(
                                "{prefix}_{}",
                                heck::AsSnakeCase(generic_parameter.to_string())
                            )
                            .as_str(),
                            generic_parameter.span(),
                        )
                    };

                    let map = {
                        let method_name = method_name("map");
                        let target_parameters = replace_param_with(
                            generic_params.clone(),
                            generic_parameter,
                            &target_parameter,
                        );
                        let variants = variants.iter().map(
                            |Variant {
                                 ident: variant,
                                 fields,
                                 ..
                             }| {
                                match fields {
                                    syn::Fields::Named(_) => {
                                        let fields_mapping = field_types(fields.iter())
                                            .into_iter()
                                            .map(|(field, ty)| match &ty == generic_parameter {
                                                false => quote! {
                                                    #field
                                                },
                                                true => quote! {
                                                    #field: #method_name(#field)
                                                },
                                            })
                                            .collect::<Vec<_>>();
                                        let field_names = field_types(fields.iter())
                                            .into_iter()
                                            .map(|(field_name, _)| field_name)
                                            .collect::<Vec<_>>();
                                        quote! {
                                            #struct_name::#variant {
                                                #(#field_names,)*
                                            } => #struct_name::#variant {
                                                #(#fields_mapping,)*
                                            }
                                        }
                                    }
                                    syn::Fields::Unnamed(_) => {
                                        let field_names = field_types(fields.iter())
                                            .iter()
                                            .enumerate()
                                            .map(|(idx, (_, ident))| {
                                                format!("__field_{idx}")
                                                    .pipe_ref(|f| Ident::new(f, ident.span()))
                                            })
                                            .collect::<Vec<_>>();
                                        let fields_mapping = field_types(fields.iter())
                                            .into_iter()
                                            .zip(field_names.iter())
                                            .map(|((_, ty), field)| {
                                                match &ty == generic_parameter {
                                                    false => quote! {
                                                        #field
                                                    },
                                                    true => quote! {
                                                        #method_name(#field)
                                                    },
                                                }
                                            })
                                            .collect::<Vec<_>>();
                                        quote! {
                                            #struct_name::#variant (
                                                #(#field_names,)*
                                            ) => #struct_name::#variant (
                                                #(#fields_mapping,)*
                                            )
                                        }
                                    }
                                    syn::Fields::Unit => quote! {
                                        #struct_name::#variant => #struct_name::#variant
                                    },
                                }
                            },
                        );

                        quote! {
                            #vis fn #method_name<
                                #target_parameter,
                                F: FnMut(#generic_parameter) ->
                                    #target_parameter,

                           >(
                                self,
                                mut #method_name: F,
                            ) -> #struct_name<#target_parameters>
                                 {
                                match self {
                                    #(#variants,)*
                                }
                            }
                        }
                    };
                    let try_map = {
                        let method_name = method_name("try_map");
                        let target_parameters = replace_param_with(
                            generic_params.clone(),
                            generic_parameter,
                            &target_parameter,
                        );
                        let variants = variants.iter().map(
                            |Variant {
                                 ident: variant,
                                 fields,
                                 ..
                             }| {
                                match fields {
                                    syn::Fields::Named(_) => {
                                        let fields_try_mapping = field_types(fields.iter())
                                            .into_iter()
                                            .map(|(field, ty)| match &ty == generic_parameter {
                                                false => quote! {
                                                    #field
                                                },
                                                true => quote! {
                                                    #field: #method_name(#field)?
                                                },
                                            })
                                            .collect::<Vec<_>>();
                                        let field_names = field_types(fields.iter())
                                            .into_iter()
                                            .map(|(field_name, _)| field_name)
                                            .collect::<Vec<_>>();
                                        quote! {
                                            #struct_name::#variant {
                                                #(#field_names,)*
                                            } => #struct_name::#variant {
                                                #(#fields_try_mapping,)*
                                            }
                                        }
                                    }
                                    syn::Fields::Unnamed(_) => {
                                        let field_names = field_types(fields.iter())
                                            .iter()
                                            .enumerate()
                                            .map(|(idx, (_, ident))| {
                                                format!("__field_{idx}")
                                                    .pipe_ref(|f| Ident::new(f, ident.span()))
                                            })
                                            .collect::<Vec<_>>();
                                        let fields_try_mapping = field_types(fields.iter())
                                            .into_iter()
                                            .zip(field_names.iter())
                                            .map(|((_, ty), field)| {
                                                match &ty == generic_parameter {
                                                    false => quote! {
                                                        #field
                                                    },
                                                    true => quote! {
                                                        #method_name(#field)?
                                                    },
                                                }
                                            })
                                            .collect::<Vec<_>>();
                                        quote! {
                                            #struct_name::#variant (
                                                #(#field_names,)*
                                            ) => #struct_name::#variant (
                                                #(#fields_try_mapping,)*
                                            )
                                        }
                                    }
                                    syn::Fields::Unit => quote! {
                                        #struct_name::#variant => #struct_name::#variant
                                    },
                                }
                            },
                        );

                        quote! {
                            #vis fn #method_name<
                                #target_parameter,
                                #err_parameter,
                                F: FnMut(#generic_parameter) -> std::result::Result<
                                    #target_parameter,
                                    #err_parameter,
                                >,
                           >(
                                self,
                                mut #method_name: F,
                            ) -> std::result::Result<
                                    #struct_name<#target_parameters>,
                                    #err_parameter,
                                > {
                                Ok(match self {
                                    #(#variants,)*
                                })
                            }
                        }
                    };

                    quote! {
                        #[automatically_derived]
                        impl <#generic_params> #struct_name<#generic_params> {
                            #map
                            #try_map
                        }
                    }
                }
                syn::Data::Union(_) => unimplemented!("unions are not supported"),
            }
        })
        .collect::<Vec<_>>();
    quote! {
        #(#impls)*
    }
    .into()
}

fn type_ident(field: &Field) -> Option<Ident> {
    match &field.ty {
        Type::Path(TypePath { path, .. }) => {
            path.segments.last().cloned().map(|segment| segment.ident)
        }
        _ => None,
    }
}
