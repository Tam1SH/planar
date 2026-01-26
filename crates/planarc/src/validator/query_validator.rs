use crate::{
    linker::linked_ast::{LinkedMatchQueryReference, LinkedModule, LinkedNodeStatement},
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

        let global_lang = if let Some(grammar_ref) = &module.grammar {
            if let Some(lang_name) = grammar_ref.value.strip_prefix("grammars.") {
                match self.grammars.get_language(lang_name) {
                    Ok(lang) => Some(lang),
                    Err(_) => {
                        let (src, span) = self.registry.get_source_and_span(grammar_ref.loc);
                        errors.push(Box::new(ValidationError::GrammarNotFound {
                            name: lang_name.to_string(),
                            span,
                            src,
                            loc: grammar_ref.loc,
                        }));
                        None
                    }
                }
            } else {
                let (src, span) = self.registry.get_source_and_span(grammar_ref.loc);
                errors.push(Box::new(ValidationError::InvalidGrammarNamespace {
                    namespace: grammar_ref.value.clone(),
                    span,
                    src,
                    loc: grammar_ref.loc,
                }));
                None
            }
        } else {
            None
        };

        for node in &module.nodes {
            for stmt in &node.value.statements {
                if let LinkedNodeStatement::Match(m) = &stmt
                    && let LinkedMatchQueryReference::Raw { source: sourse, .. } =
                        &m.value.query_ref.value
                {
                    if let Some(lang) = &global_lang {
                        if let Err(e) = tree_sitter::Query::new(lang, &sourse.value) {
                            let (src, span) =
                                self.registry.get_source_and_span(m.value.query_ref.loc);
                            errors.push(Box::new(ValidationError::InvalidQuerySyntax {
                                message: e.to_string(),
                                span,
                                src,
                                loc: m.value.query_ref.loc,
                            }));
                        }
                    } else {
                        let (src, span) = self.registry.get_source_and_span(m.value.query_ref.loc);
                        errors.push(Box::new(ValidationError::UntypedQuery {
                            span,
                            src,
                            loc: m.value.query_ref.loc,
                        }));
                    }
                }
            }
        }

        for query in &module.queries {
            if let Some(lang) = &global_lang {
                if let Err(e) = tree_sitter::Query::new(&lang, &query.value.query) {
                    let (src, span) = self.registry.get_source_and_span(query.loc);
                    errors.push(Box::new(ValidationError::InvalidQuerySyntax {
                        message: e.to_string(),
                        span,
                        src,
                        loc: query.loc,
                    }));
                }
            } else {
                let (src, span) = self.registry.get_source_and_span(query.loc);
                errors.push(Box::new(ValidationError::UntypedQuery {
                    span,
                    src,
                    loc: query.loc,
                }));
            }
        }

        ValidationErrors::new(errors)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::linker::dependency_graph::LoweredGraph;
    use crate::linker::linked_ast::LinkedModule;
    use crate::linker::linker::link_to_world;
    use crate::linker::linker::tests::setup_lowered_graph;
    use crate::loader::{LanguageProvider, MockLanguageLoader};
    use crate::source_registry::SourceRegistry;

    fn setup_test(files: &[(&str, &str)]) -> (SourceRegistry, GrammarRegistry, LinkedModule) {
        let lg = setup_lowered_graph(files);

        let (world, _errors) = link_to_world(vec![], lg);

        let mut gr = GrammarRegistry::new(Box::new(MockLanguageLoader));
        gr.add_grammar("pdl".into(), "pdl.so".into());

        let linked_mod = world
            .modules
            .get("main")
            .cloned()
            .expect("Module 'main' should be present in linked world");

        (world.registry, gr, linked_mod)
    }

    #[test]
    fn test_query_ok() {
        let (reg, gr, m) =
            setup_test(&[("main", "using grammars.pdl\nquery Q = `(identifier) @id`")]);

        let v = QueryValidator {
            registry: &reg,
            grammars: &gr,
        };

        let errs = v.validate_module(&m);
        assert!(errs.is_empty(), "Expected no errors, got: {:?}", errs);
    }

    #[test]
    fn test_query_bad_syntax() {
        let (reg, gr, m) = setup_test(&[("main", "using grammars.pdl\nquery Q = `(unclosed`")]);

        let v = QueryValidator {
            registry: &reg,
            grammars: &gr,
        };

        let errs = v.validate_module(&m);
        assert!(!errs.0.is_empty());
        assert!(matches!(
            errs.0[0].as_ref(),
            ValidationError::InvalidQuerySyntax { .. }
        ));
    }

    #[test]
    fn test_query_bad_ns() {
        let (reg, gr, m) = setup_test(&[("main", "using bad.pdl\nquery Q = `(id)`")]);

        let v = QueryValidator {
            registry: &reg,
            grammars: &gr,
        };

        let errs = v.validate_module(&m);
        assert!(matches!(
            errs.0[0].as_ref(),
            ValidationError::InvalidGrammarNamespace { .. }
        ));
    }

    #[test]
    fn test_query_not_found() {
        let (reg, gr, m) = setup_test(&[("main", "using grammars.missing\nquery Q = `(id)`")]);

        let v = QueryValidator {
            registry: &reg,
            grammars: &gr,
        };

        let errs = v.validate_module(&m);
        assert!(matches!(
            errs.0[0].as_ref(),
            ValidationError::GrammarNotFound { .. }
        ));
    }
}
