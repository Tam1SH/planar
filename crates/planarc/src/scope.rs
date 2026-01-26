use std::collections::{BTreeMap, HashSet};

#[derive(Default)]
pub struct ScopeStack<T> {
    stack: Vec<BTreeMap<String, T>>,
}

impl<T> ScopeStack<T> {
    pub fn new() -> Self {
        Self {
            stack: vec![BTreeMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.stack.push(BTreeMap::new());
    }

    pub fn pop(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        }
    }

    pub fn define(&mut self, name: String, value: T) {
        if let Some(current) = self.stack.last_mut() {
            current.insert(name, value);
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&T> {
        for scope in self.stack.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }

    pub fn all_visible_names(&self) -> Vec<String> {
        let mut names = HashSet::new();
        for layer in &self.stack {
            for name in layer.keys() {
                names.insert(name.clone());
            }
        }
        let mut sorted: Vec<_> = names.into_iter().collect();
        sorted.sort();
        sorted
    }

    pub fn in_scope<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.push();
        let res = f(self);
        self.pop();
        res
    }
}
