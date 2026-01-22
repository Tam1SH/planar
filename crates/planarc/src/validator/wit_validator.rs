use crate::{
    linker::{
        ids::ResolvedId,
        linked_ast::{LinkedExpression, LinkedModule, LinkedTypeDefinition, LinkedTypeReference},
        symbol_table::SymbolTable,
    },
    source_registry::SourceRegistry,
    spanned::Spanned,
    validator::error::{ValidationError, ValidationErrors},
};

pub struct WitValidator<'a> {
    pub table: &'a SymbolTable,
    pub registry: &'a SourceRegistry,
}

impl<'a> WitValidator<'a> {
    pub fn validate_module(&self, module: &LinkedModule) -> ValidationErrors {
        let mut errors = Vec::new();

        for ty in &module.types {
            let is_wit = ty
                .value
                .attributes
                .iter()
                .any(|a| a.value.name.value == "wit-compatible");
            let is_export = ty
                .value
                .attributes
                .iter()
                .any(|a| a.value.name.value == "wit-export");

            if is_wit || is_export {
                self.check_type(&ty.value.name, &ty.value.definition, &mut errors);
            }
        }

        ValidationErrors(errors)
    }

    fn check_type(
        &self,
        type_name: &str,
        def: &Spanned<LinkedTypeDefinition>,
        errors: &mut Vec<ValidationError>,
    ) {
        if let Some(base) = &def.value.base_type {
            self.check_type_ref(type_name, base, errors);
        }

        for field in &def.value.fields {
            let field_def = Spanned::new(field.value.definition.clone(), field.loc);
            self.check_type(type_name, &field_def, errors);
        }
    }

    fn check_type_ref(
        &self,
        owner_name: &str,
        refer: &LinkedTypeReference,
        errors: &mut Vec<ValidationError>,
    ) {
        match &refer.symbol.value {
            ResolvedId::Global(id_spanned) => {
                if let Some(fqmn) = self.table.get_fqmn(id_spanned.value) {
                    if !self.is_wit_safe(fqmn) {
                        let (src, span) = self.registry.get_source_and_span(refer.symbol.loc);
                        errors.push(ValidationError::WitIncompatibility {
                            name: owner_name.to_string(),
                            used: fqmn.clone(),
                            span,
                            src,
                            loc: refer.symbol.loc,
                        });
                    }
                }
            }
            ResolvedId::Local(_) => {}
        }

        if let Some(re) = &refer.refinement {
            let (src, span) = self.registry.get_source_and_span(re.loc);
            errors.push(ValidationError::WitRefinementDisallowed {
                span,
                src,
                loc: refer.symbol.loc,
            });
        }

        for arg in &refer.args {
            self.check_type_ref(owner_name, &arg.value, errors);
        }
    }

    fn is_wit_safe(&self, fqmn: &str) -> bool {
        fqmn.starts_with("std.wit.")
    }
}

#[cfg(test)]
mod tests {
    use crate::linker::linker::tests::setup_project;

    use super::*;

    fn run_validation(files: &[(&str, &str)]) -> ValidationErrors {
        let mut all_files = files.to_vec();
        all_files.push(("std.wit", "pub type WitResource = builtin.i64\n pub type WitInt = builtin.i64\n pub type WitStr = builtin.str"));

        let (lg, mut linker) = setup_project(&all_files, "main");

        linker.prelude.push("std.wit".to_string());

        let (linked_mod, linker_errs) =
            linker.link_module("main", &lg.modules["main"], &lg.registry);
        assert!(linker_errs.is_empty(), "Linker errors: {:?}", linker_errs);

        let validator = WitValidator {
            table: &linker.table,
            registry: &lg.registry,
        };

        validator.validate_module(&linked_mod)
    }

    #[test]
    fn test_wit_compatible_primitives_pass() {
        let files = [(
            "main",
            r#"
                #wit-export
                #wit-compatible
                type User = {
                    name: std.wit.WitStr
                    age: std.wit.WitInt
                }
            "#,
        )];

        let errors = run_validation(&files);
        assert!(errors.is_empty(), "Should be valid: {:?}", errors);
    }

    #[test]
    fn test_wit_export_fails_on_non_wit_type() {
        let files = [(
            "main",
            r#"
                type NotValid = builtin.i64

                #wit-export
                #wit-compatible
                type User = {
                    name: std.wit.WitStr
                    age: NotValid
                }
            "#,
        )];

        let errors = run_validation(&files);
        let error = errors.0.first().unwrap();

        assert!(matches!(error, ValidationError::WitIncompatibility { .. }));
    }

    #[test]
    fn test_wit_export_fails_on_refinement() {
        let files = [(
            "main",
            r#"
                extern {
                    operator > left: builtin.str, right: builtin.str -> builtin.str
                }
                
                #wit-export
                type PositiveInt = std.wit.WitInt where it > 0
            "#,
        )];

        let errors = run_validation(&files);
        assert!(
            errors
                .0
                .iter()
                .any(|e| matches!(e, ValidationError::WitRefinementDisallowed { .. })),
            "Refinements must be strictly forbidden in WIT exports"
        );
    }

    #[test]
    fn test_wit_generic_argument_must_be_wit_compatible() {
        let files = [(
            "main",
            r#"
                import std.wit
                type NonWit = builtin.f64

                #wit-export
                type Data = {
                    values: builtin.list NonWit
                }
            "#,
        )];

        let errors = run_validation(&files);
        assert!(
            !errors.0.is_empty(),
            "Generic argument NonWit is not from std.wit"
        );
    }
}
