use miette::{Diagnostic, SourceSpan};
use std::fmt::Write;
use thiserror::Error;

use crate::{
    ast::{FactDefinition, FactField, TypeAnnotation},
    spanned::Spanned,
};

#[derive(Error, Diagnostic, Debug)]
pub enum SchemaError {
    #[error("Unsupported type for database schema: {ty}")]
    #[diagnostic(
        code(pdl::fact::unsupported_type),
        help("Kuzu supports: String, Int, Float, Bool, List<T>")
    )]
    UnsupportedType {
        ty: String,
        #[label("this type")]
        span: SourceSpan,
    },

    #[error("Missing ID definition for fact '{fact_name}'")]
    #[diagnostic(
        code(pdl::fact::missing_id),
        help("Add #[id] to a field or #[auto_id] to the fact definition")
    )]
    MissingId {
        fact_name: String,
        #[label("fact defined here")]
        span: SourceSpan,
    },

    #[error("Ambiguous ID definition")]
    #[diagnostic(
        code(pdl::fact::ambiguous_id),
        help("Cannot use #[auto_id] and field-level #[id] simultaneously")
    )]
    AmbiguousId {
        #[label("conflicting #[auto_id]")]
        auto_id_span: SourceSpan,
        #[label("conflicting field #[id]")]
        field_id_span: SourceSpan,
    },

    #[error("Generic type mismatch")]
    #[diagnostic(code(pdl::fact::generic_mismatch))]
    GenericMismatch {
        message: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Format error")]
    FmtError(#[from] std::fmt::Error),
}

pub struct KuzuSchemaBuilder;

impl KuzuSchemaBuilder {
    pub fn build(fact: &FactDefinition) -> Result<String, SchemaError> {
        let table_name = Self::sanitize(&fact.name.value);

        let (pk_column, extra_columns) = Self::resolve_primary_key(fact)?;

        let mut columns_ddl = Vec::new();

        for field in fact.fields.iter().map(|f| &f.value) {
            let col_name = Self::sanitize(&field.name.value);
            let col_type = Self::map_type(&field.ty)?;
            columns_ddl.push(format!("{} {}", col_name, col_type));
        }

        for (col_name, col_type) in extra_columns {
            columns_ddl.push(format!("{} {}", Self::sanitize(&col_name), col_type));
        }

        let mut query = String::new();
        write!(query, "CREATE NODE TABLE {} (", table_name)?;

        query.push_str(&columns_ddl.join(", "));

        write!(query, ", PRIMARY KEY ({}))", Self::sanitize(&pk_column))?;

        Ok(query)
    }

    fn resolve_primary_key(
        fact: &FactDefinition,
    ) -> Result<(String, Vec<(String, String)>), SchemaError> {
        let auto_id_attr = fact
            .attributes
            .iter()
            .find(|a| a.value.name.value == "auto_id");

        let id_fields: Vec<&Spanned<FactField>> = fact
            .fields
            .iter()
            .filter(|f| {
                f.value
                    .attributes
                    .iter()
                    .any(|a| a.value.name.value == "id")
            })
            .collect();

        match (auto_id_attr, id_fields.len()) {
            (Some(auto), count) if count > 0 => Err(SchemaError::AmbiguousId {
                auto_id_span: auto.loc.into(),
                field_id_span: id_fields[0].loc.into(),
            }),

            (Some(_), 0) => {
                let pk_col = "_xxhash_id".to_string();
                Ok((pk_col.clone(), vec![(pk_col, "INT64".to_string())]))
            }

            (None, 1) => {
                let field = id_fields[0];
                Ok((field.value.name.value.clone(), vec![]))
            }

            (None, count) if count > 1 => {
                let pk_col = "_composite_id_hash".to_string();
                Ok((pk_col.clone(), vec![(pk_col, "INT64".to_string())]))
            }

            (None, 0) => Err(SchemaError::MissingId {
                fact_name: fact.name.value.clone(),
                span: fact.name.loc.into(),
            }),

            _ => unreachable!(),
        }
    }

    fn map_type(ty: &TypeAnnotation) -> Result<String, SchemaError> {
        let type_name = ty.name.value.as_str();

        match type_name {
            "String" | "str" | "Text" => Ok("STRING".to_string()),
            "Int" | "i64" | "Integer" => Ok("INT64".to_string()),
            "Int32" | "i32" => Ok("INT32".to_string()),
            "Float" | "f64" | "Double" => Ok("DOUBLE".to_string()),
            "Float32" | "f32" => Ok("FLOAT".to_string()),
            "Bool" | "Boolean" => Ok("BOOLEAN".to_string()),
            "Date" => Ok("DATE".to_string()),
            "Timestamp" => Ok("TIMESTAMP".to_string()),
            "Interval" => Ok("INTERVAL".to_string()),
            "List" => {
                if ty.args.len() != 1 {
                    return Err(SchemaError::GenericMismatch {
                        message: format!("List expects 1 argument, found {}", ty.args.len()),
                        span: ty.name.loc.into(),
                    });
                }

                let inner = &ty.args[0].value;
                let inner_kuzu = Self::map_type(inner)?;
                Ok(format!("{}[]", inner_kuzu))
            }

            _ => Err(SchemaError::UnsupportedType {
                ty: type_name.to_string(),
                span: ty.name.loc.into(),
            }),
        }
    }

    fn sanitize(ident: &str) -> String {
        if ident.is_empty() {
            return "``".to_string();
        }
        format!("`{}`", ident.replace('`', "``"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::Attribute,
        spanned::{FileId, Location, Spanned},
    };

    fn sp<T>(value: T) -> Spanned<T> {
        Spanned {
            loc: Location::new(FileId(0), (0usize..0usize).into()),
            value,
        }
    }

    fn s(val: &str) -> Spanned<String> {
        sp(val.to_string())
    }

    fn ty_simple(name: &str) -> TypeAnnotation {
        TypeAnnotation {
            name: s(name),
            args: vec![],
            refinement: None,
        }
    }

    fn ty_list(inner: TypeAnnotation) -> TypeAnnotation {
        TypeAnnotation {
            name: s("List"),
            args: vec![sp(inner)],
            refinement: None,
        }
    }

    fn attr(name: &str) -> Spanned<Attribute> {
        sp(Attribute { name: s(name) })
    }

    fn field(name: &str, ty: TypeAnnotation, attrs: Vec<&str>) -> Spanned<FactField> {
        let attributes = attrs.into_iter().map(attr).collect();
        sp(FactField {
            attributes,
            name: s(name),
            ty,
        })
    }

    fn fact(name: &str, fields: Vec<Spanned<FactField>>, fact_attrs: Vec<&str>) -> FactDefinition {
        let attributes = fact_attrs.into_iter().map(attr).collect();
        FactDefinition {
            attributes,
            name: s(name),
            fields,
            vis: Default::default(),
        }
    }

    #[test]
    fn test_table_name_injection() {
        let malicious_name = "Users` (id INT64); DROP NODE TABLE Users; --";

        let f = fact(
            malicious_name,
            vec![field("id", ty_simple("Int"), vec!["id"])],
            vec![],
        );

        let ddl = KuzuSchemaBuilder::build(&f).expect("Should build safely");

        assert_eq!(
            ddl,
            "CREATE NODE TABLE `Users`` (id INT64); DROP NODE TABLE Users; --` (`id` INT64, PRIMARY KEY (`id`))"
        );
    }

    #[test]
    fn test_field_name_injection() {
        let malicious_field = "price INT64, isAdmin BOOLEAN";

        let f = fact(
            "Product",
            vec![
                field("id", ty_simple("String"), vec!["id"]),
                field(malicious_field, ty_simple("Int"), vec![]),
            ],
            vec![],
        );

        let ddl = KuzuSchemaBuilder::build(&f).expect("Should build safely");

        assert_eq!(
            ddl,
            "CREATE NODE TABLE `Product` (`id` STRING, `price INT64, isAdmin BOOLEAN` INT64, PRIMARY KEY (`id`))"
        );
    }

    #[test]
    fn test_backtick_escaping() {
        let weird_name = "Weird`Name";
        let f = fact(
            weird_name,
            vec![field("id", ty_simple("Int"), vec!["id"])],
            vec![],
        );

        let ddl = KuzuSchemaBuilder::build(&f).unwrap();

        assert_eq!(
            ddl,
            "CREATE NODE TABLE `Weird``Name` (`id` INT64, PRIMARY KEY (`id`))"
        );
    }

    #[test]
    fn test_missing_id_error() {
        let f = fact(
            "NoIdFact",
            vec![field("name", ty_simple("String"), vec![])],
            vec![],
        );

        let res = KuzuSchemaBuilder::build(&f);

        match res {
            Err(SchemaError::MissingId { fact_name, .. }) => {
                assert_eq!(fact_name, "NoIdFact");
            }
            _ => panic!("Expected MissingId error, got {:?}", res),
        }
    }

    #[test]
    fn test_ambiguous_id_error() {
        let f = fact(
            "ConfusedFact",
            vec![field("my_id", ty_simple("Int"), vec!["id"])],
            vec!["auto_id"],
        );

        let res = KuzuSchemaBuilder::build(&f);

        match res {
            Err(SchemaError::AmbiguousId { .. }) => {}
            _ => panic!("Expected AmbiguousId error, got {:?}", res),
        }
    }

    #[test]
    fn test_unsupported_type() {
        let f = fact(
            "BadTypeFact",
            vec![field("meta", ty_simple("Dict"), vec!["id"])],
            vec![],
        );

        let res = KuzuSchemaBuilder::build(&f);

        match res {
            Err(SchemaError::UnsupportedType { ty, .. }) => {
                assert_eq!(ty, "Dict");
            }
            _ => panic!("Expected UnsupportedType error, got {:?}", res),
        }
    }

    #[test]
    fn test_generic_mismatch_count() {
        let invalid_list_ty = TypeAnnotation {
            name: s("List"),
            args: vec![sp(ty_simple("String")), sp(ty_simple("Int"))],
            refinement: None,
        };

        let f = fact(
            "BadList",
            vec![field("items", invalid_list_ty, vec!["id"])],
            vec![],
        );

        let res = KuzuSchemaBuilder::build(&f);

        match res {
            Err(SchemaError::GenericMismatch { message, .. }) => {
                assert!(message.contains("expects 1 argument"));
            }
            _ => panic!("Expected GenericMismatch error, got {:?}", res),
        }
    }

    #[test]
    fn test_auto_id_strategy() {
        let f = fact(
            "Log",
            vec![field("msg", ty_simple("String"), vec![])],
            vec!["auto_id"],
        );

        let ddl = KuzuSchemaBuilder::build(&f).unwrap();

        assert!(ddl.contains("CREATE NODE TABLE `Log`"));
        assert!(ddl.contains("`_xxhash_id` INT64"));
        assert!(ddl.contains("PRIMARY KEY (`_xxhash_id`)"));
    }

    #[test]
    fn test_composite_id_strategy() {
        let f = fact(
            "OrderItem",
            vec![
                field("order_id", ty_simple("String"), vec!["id"]),
                field("item_index", ty_simple("Int"), vec!["id"]),
                field("amount", ty_simple("Float"), vec![]),
            ],
            vec![],
        );

        let ddl = KuzuSchemaBuilder::build(&f).unwrap();

        assert!(ddl.contains("CREATE NODE TABLE `OrderItem`"));
        assert!(ddl.contains("`order_id` STRING"));
        assert!(ddl.contains("`item_index` INT64"));
        assert!(ddl.contains("`_composite_id_hash` INT64"));
        assert!(ddl.contains("PRIMARY KEY (`_composite_id_hash`)"));
    }

    #[test]
    fn test_complex_types_mapping() {
        let inner_list = ty_list(ty_simple("Int"));
        let outer_list = ty_list(inner_list);

        let f = fact(
            "Matrix",
            vec![field("data", outer_list, vec!["id"])],
            vec![],
        );

        let ddl = KuzuSchemaBuilder::build(&f).unwrap();

        assert!(ddl.contains("`data` INT64[][]"));
    }
}
