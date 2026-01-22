use crate::{
    linker::linked_ast::LinkedModule,
    source_registry::SourceRegistry,
    validator::{
        error::{ValidationError, ValidationErrors},
        grammar_registry::GrammarRegistry,
    },
};

pub struct QueryValidator<'a> {
    pub registry: &'a SourceRegistry,
    pub grammars: &'a GrammarRegistry,
}

impl<'a> QueryValidator<'a> {
    pub fn validate_module(&self, module: &LinkedModule) -> ValidationErrors {
        let mut errors = Vec::new();

        for query in &module.queries {
            if let Some(lang_name) = query.value.grammar.strip_prefix("grammars.") {
                match self.grammars.get_language(lang_name) {
                    Ok(lang) => {
                        if let Err(e) = tree_sitter::Query::new(&lang, &query.value.query) {
                            let (src, span) = self.registry.get_source_and_span(query.loc);
                            errors.push(ValidationError::InvalidQuerySyntax {
                                message: e.to_string(),
                                span,
                                src,
                                loc: query.loc,
                            });
                        }
                    }
                    Err(_) => {
                        let (src, span) = self.registry.get_source_and_span(query.loc);
                        errors.push(ValidationError::GrammarNotFound {
                            name: lang_name.to_string(),
                            span,
                            src,
                            loc: query.loc,
                        });
                    }
                }
            } else {
                let (src, span) = self.registry.get_source_and_span(query.loc);
                errors.push(ValidationError::InvalidGrammarNamespace {
                    namespace: query.value.grammar.clone(),
                    span,
                    src,
                    loc: query.loc,
                });
            }
        }

        ValidationErrors(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linker::dependency_graph::LoweredGraph;
    use crate::linker::linker::tests::setup_project;
    use crate::loader::{LanguageProvider, MockLanguageLoader};
    use crate::module_loader::Source;
    use std::path::PathBuf;

    fn setup_test(files: &[(&str, &str)]) -> (LoweredGraph, GrammarRegistry, LinkedModule) {
        let (lg, mut linker) = setup_project(files, "main");
        let (linked_mod, _) = linker.link_module("main", &lg.modules["main"], &lg.registry);

        let mut gr = GrammarRegistry::new(Box::new(MockLanguageLoader));
        gr.add_grammar("pdl".into(), "pdl.so".into());

        (lg, gr, linked_mod)
    }

    #[test]
    fn test_query_ok() {
        let (lg, gr, m) = setup_test(&[("main", "query Q: grammars.pdl = `(identifier) @id`")]);
        let v = QueryValidator {
            registry: &lg.registry,
            grammars: &gr,
        };

        assert!(v.validate_module(&m).is_empty());
    }

    #[test]
    fn test_query_bad_syntax() {
        let (lg, gr, m) = setup_test(&[("main", "query Q: grammars.pdl = `(unclosed`")]);
        let v = QueryValidator {
            registry: &lg.registry,
            grammars: &gr,
        };

        let errs = v.validate_module(&m);
        assert!(matches!(
            errs.0[0],
            ValidationError::InvalidQuerySyntax { .. }
        ));
    }

    #[test]
    fn test_query_bad_ns() {
        let (lg, gr, m) = setup_test(&[("main", "query Q: bad.pdl = `(id)`")]);
        let v = QueryValidator {
            registry: &lg.registry,
            grammars: &gr,
        };

        let errs = v.validate_module(&m);
        assert!(matches!(
            errs.0[0],
            ValidationError::InvalidGrammarNamespace { .. }
        ));
    }

    #[test]
    fn test_query_not_found() {
        let (lg, gr, m) = setup_test(&[("main", "query Q: grammars.missing = `(id)`")]);
        let v = QueryValidator {
            registry: &lg.registry,
            grammars: &gr,
        };

        let errs = v.validate_module(&m);
        assert!(matches!(errs.0[0], ValidationError::GrammarNotFound { .. }));
    }
}
