use typst_shim::syntax::LinkedNodeExt;

use crate::{
    adt::interner::Interned,
    prelude::*,
    syntax::{classify_context, classify_syntax, ArgClass, SyntaxContext},
    SemanticRequest,
};

/// The [`textDocument/signatureHelp`] request is sent from the client to the
/// server to request signature information at a given cursor position.
///
/// [`textDocument/signatureHelp`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_signatureHelp
#[derive(Debug, Clone)]
pub struct SignatureHelpRequest {
    /// The path of the document to get signature help for.
    pub path: PathBuf,
    /// The position of the cursor to get signature help for.
    pub position: LspPosition,
}

impl SemanticRequest for SignatureHelpRequest {
    type Response = SignatureHelp;

    fn request(self, ctx: &mut LocalContext) -> Option<Self::Response> {
        let source = ctx.source_by_path(&self.path).ok()?;
        let cursor = ctx.to_typst_pos(self.position, &source)? + 1;

        let ast_node = LinkedNode::new(source.root()).leaf_at_compat(cursor)?;
        let SyntaxContext::Arg {
            callee,
            target,
            is_set,
            ..
        } = classify_context(ast_node, Some(cursor))?
        else {
            return None;
        };

        let syntax = classify_syntax(callee, cursor)?;
        let def = ctx.def_of_syntax_or_dyn(&source, None, syntax)?;
        let sig = ctx.sig_of_def(def.clone())?;
        crate::log_debug_ct!("got signature {sig:?}");

        let param_shift = sig.param_shift();
        let mut active_parameter = None;

        let mut label = def.name().as_ref().to_owned();
        let mut params = Vec::new();

        label.push('(');

        let mut real_offset = 0;
        let focus_name = OnceLock::new();
        for (idx, (param, ty)) in sig.params().enumerate() {
            if is_set && !param.attrs.settable {
                continue;
            }

            match &target {
                ArgClass::Positional { .. } if is_set => {}
                ArgClass::Positional { positional, .. } => {
                    if (*positional) + param_shift == idx {
                        active_parameter = Some(real_offset);
                    }
                }
                ArgClass::Named(name) => {
                    let focus_name = focus_name
                        .get_or_init(|| Interned::new_str(&name.get().clone().into_text()));
                    if focus_name == &param.name {
                        active_parameter = Some(real_offset);
                    }
                }
            }

            real_offset += 1;

            if !params.is_empty() {
                label.push_str(", ");
            }

            label.push_str(&format!(
                "{}: {}",
                param.name,
                ty.unwrap_or(&param.ty)
                    .describe()
                    .as_deref()
                    .unwrap_or("any")
            ));

            params.push(ParameterInformation {
                label: lsp_types::ParameterLabel::Simple(format!("{}:", param.name)),
                documentation: param.docs.as_ref().map(|docs| {
                    Documentation::MarkupContent(MarkupContent {
                        value: docs.as_ref().into(),
                        kind: MarkupKind::Markdown,
                    })
                }),
            });
        }
        label.push(')');
        let ret = sig.type_sig().body.clone();
        if let Some(ret_ty) = ret {
            label.push_str(" -> ");
            label.push_str(ret_ty.describe().as_deref().unwrap_or("any"));
        }

        if matches!(target, ArgClass::Positional { .. }) {
            active_parameter =
                active_parameter.map(|x| x.min(sig.primary().pos_size().saturating_sub(1)));
        }

        crate::log_debug_ct!("got signature info {label} {params:?}");

        Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label: label.to_string(),
                documentation: sig.primary().docs.as_deref().map(markdown_docs),
                parameters: Some(params),
                active_parameter: active_parameter.map(|x| x as u32),
            }],
            active_signature: Some(0),
            active_parameter: None,
        })
    }
}

fn markdown_docs(docs: &str) -> Documentation {
    Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: docs.to_owned(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn test() {
        snapshot_testing("signature_help", &|ctx, path| {
            let source = ctx.source_by_path(&path).unwrap();
            let (position, anno) = make_pos_annotation(&source);

            let request = SignatureHelpRequest { path, position };

            let result = request.request(ctx);
            with_settings!({
                description => format!("signature help on {anno}"),
            }, {
                assert_snapshot!(JsonRepr::new_redacted(result, &REDACT_LOC));
            })
        });
    }
}
