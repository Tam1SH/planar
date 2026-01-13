use std::{collections::HashMap, env};

#[derive(Default, Clone, Debug)]
pub struct VarRegistry {
    pub(crate) vars: HashMap<String, String>,
}

impl VarRegistry {
    pub fn new() -> Self {
        let mut vars = HashMap::new();
        Self { vars }
    }

    pub fn resolve(&self, key: &str, source_type: &str) -> Option<String> {
        match source_type {
            "env" => env::var(key).ok(),
            "var" => self.vars.get(key).cloned(),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use kdl::KdlDocument;
    use planar_config_macro::Parser;

    use crate::{
        parser::{ctx::ParseContext, parsable::KdlParsable},
        var_registry::VarRegistry,
    };

    #[test]
    fn test_threads_from_sys_var() {
        let val = 8;

        #[derive(Parser)]
        #[node(root)]
        struct Test {
            #[node(child)]
            pub sstruct: SystemTest,
        }

        #[derive(Parser)]
        struct SystemTest {
            #[node(child)]
            pub val: usize,
        }

        let input = r#"
            sstruct {
                val (var)"val" 
            }
        "#;
        let mut reg = VarRegistry::new();
        reg.vars.insert("val".to_string(),  val.to_string());

        let ctx = ParseContext::new_with_registry(
            input.parse::<KdlDocument>().unwrap().into(),
            "<test>".into(),
            reg.into(),
        );

        let test = Test::parse_node(&ctx, &()).unwrap();

        assert_eq!(test.sstruct.val, val);
    }
}
