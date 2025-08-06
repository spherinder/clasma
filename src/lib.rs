use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Attribute, FnArg, ItemFn, Pat, Type};

fn is_clasma_field_attr(attr: &Attribute) -> bool {
    let Some(first_seg) = attr.path().segments.first() else { return false };
    first_seg.ident == "clasma"
}

#[proc_macro_attribute]
pub fn partial(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut func = parse_macro_input!(item as ItemFn);

    let func_name = &func.sig.ident;

    let mut match_args = Vec::new();
    let mut expan_args = Vec::new();
    'l: for (i,arg) in func.sig.inputs.iter().enumerate() {
        'attr: {
            let FnArg::Typed(pat_type) = arg else { break 'attr };
            let Type::Reference(refty) = &*pat_type.ty else { break 'attr };
            if !pat_type.attrs.iter().any(is_clasma_field_attr) { break 'attr };
            let Pat::Ident(pat_ident) = &*pat_type.pat else { break 'attr };
            let field = &pat_ident.ident;
            expan_args.push(match refty.mutability {
                Some(_) => quote! { &mut $(st).#field },
                None => quote! { &$(st).#field },
            });
            continue 'l;
        }
        let matcharg = format_ident!("__arg{i}");
        match_args.push(quote! { $#matcharg:expr });
        expan_args.push(quote! { $#matcharg });
    }

    for arg in func.sig.inputs.iter_mut() {
        if let FnArg::Typed(pat_type) = arg {
            pat_type.attrs.retain(|a| !is_clasma_field_attr(a));
        }
    }

    let res = quote! {
        #func

        #[macro_export]
        macro_rules! #func_name {
            ( <$($generic:tt),+>, $st:expr, #(#match_args),* ) => {
                #func_name::<$($generic),*>( #(#expan_args),* );
            };
            ( $st:expr, #(#match_args),* ) => {
                #func_name( #(#expan_args),* );
            };
        }
    };

    res.into()
}
