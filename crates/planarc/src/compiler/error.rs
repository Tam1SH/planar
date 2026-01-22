use miette::Diagnostic;
use std::fmt;
use thiserror::Error;

use crate::spanned::Location;

pub trait ErrorWithLocation {
    fn location(&self) -> Location;
}

pub trait DiagnosticWithLocation: Diagnostic + ErrorWithLocation {}

impl<T: Diagnostic + ErrorWithLocation> DiagnosticWithLocation for T {}

#[derive(Error, Default)]
#[error("Compilation failed with {} errors", .0.len())]
pub struct CompilersError(pub Vec<Box<dyn DiagnosticWithLocation + Send + Sync + 'static>>);

impl CompilersError {
    pub fn push<E>(&mut self, error: E)
    where
        E: DiagnosticWithLocation + Send + Sync + 'static,
    {
        self.0.push(Box::new(error));
    }

    pub fn extend<E>(&mut self, errors: Vec<E>)
    where
        E: DiagnosticWithLocation + Send + Sync + 'static,
    {
        for err in errors {
            self.0.push(Box::new(err));
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Diagnostic for CompilersError {
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        if self.0.is_empty() {
            None
        } else {
            let iter = self.0.iter().map(|err| err.as_ref() as &dyn Diagnostic);
            Some(Box::new(iter))
        }
    }
}

impl fmt::Debug for CompilersError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&miette::Report::new(unsafe { std::ptr::read(self) }), f)
    }
}
