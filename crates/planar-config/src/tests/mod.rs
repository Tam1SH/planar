
#[cfg(test)]
mod tests {
    use kdl::KdlDocument;

    use crate::parser::ctx::ParseContext;
    use crate::{model::PlanarConfigDef, schema::schema_context::SchemaContext};
    use crate::schema::definitions::GetSchema;
    use crate::parser::parsable::KdlParsable;
    
    #[test]
    fn test_config() {
        let doc: KdlDocument = include_str!("./fixtures/config.kdl").parse().unwrap();
        
        let ctx = ParseContext::new(doc, "<test>");
        let result = PlanarConfigDef::parse_node(&ctx, &()).unwrap();
        
        insta::assert_debug_snapshot!(result);
    }
    #[test]
    fn test_schema_serialization_snapshot() {
        let mut ctx = SchemaContext::default();
        let schema = PlanarConfigDef::schemas(&mut ctx);
        insta::assert_yaml_snapshot!(schema);
    }
}