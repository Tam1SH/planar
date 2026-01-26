use crate::spanned::Location;
use miette::{Diagnostic, GraphicalReportHandler};
use std::fmt::{self, Debug, Display};
use thiserror::Error;

pub trait DiagnosticWithLocation:
    Diagnostic + ErrorWithLocation + Display + Send + Sync + 'static
{
}

impl<T: Diagnostic + ErrorWithLocation + Display + Send + Sync + 'static> DiagnosticWithLocation
    for T
{
}

pub trait ErrorWithLocation {
    fn location(&self) -> Location;
}

#[derive(Error)]
pub struct ErrorCollection<E>(pub Vec<Box<E>>)
where
    E: DiagnosticWithLocation;

impl<E> ErrorCollection<E>
where
    E: DiagnosticWithLocation,
{
    pub fn new(errors: Vec<Box<E>>) -> Self {
        Self(errors)
    }

    pub fn push(&mut self, error: Box<E>) {
        self.0.push(error);
    }

    pub fn extend(&mut self, other: Vec<Box<E>>) {
        self.0.extend(other);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn merge(&mut self, other: Self) {
        self.0.extend(other.0);
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<E> Default for ErrorCollection<E>
where
    E: DiagnosticWithLocation,
{
    fn default() -> Self {
        Self::new(vec![])
    }
}
impl<E> IntoIterator for ErrorCollection<E>
where
    E: DiagnosticWithLocation,
{
    type Item = Box<E>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, E> IntoIterator for &'a ErrorCollection<E>
where
    E: DiagnosticWithLocation,
{
    type Item = &'a E;
    type IntoIter = std::iter::Map<std::slice::Iter<'a, Box<E>>, fn(&'a Box<E>) -> &'a E>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().map(|b| b.as_ref())
    }
}

impl<E> Diagnostic for ErrorCollection<E>
where
    E: DiagnosticWithLocation,
{
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        if self.0.is_empty() {
            None
        } else {
            Some(Box::new(
                self.0.iter().map(|e| e.as_ref() as &dyn Diagnostic),
            ))
        }
    }
}

impl<E> fmt::Debug for ErrorCollection<E>
where
    E: DiagnosticWithLocation,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "ErrorCollection (empty)");
        }

        let handler = GraphicalReportHandler::new_themed(miette::GraphicalTheme::unicode());

        for err in &self.0 {
            handler.render_report(f, err.as_ref())?;
            writeln!(f)?;
        }

        Ok(())
    }
}

impl<E> fmt::Display for ErrorCollection<E>
where
    E: DiagnosticWithLocation,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} error{} occurred",
            self.0.len(),
            if self.0.len() == 1 { "" } else { "s" }
        )?;

        Ok(())
    }
}

#[derive(Error, Default)]
pub struct AnyErrorCollection(pub Vec<Box<dyn DiagnosticWithLocation>>);

impl AnyErrorCollection {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push<E>(&mut self, error: E)
    where
        E: DiagnosticWithLocation,
    {
        self.0.push(Box::new(error));
    }

    pub fn absorb_all<I>(&mut self, collections: I)
    where
        I: IntoIterator<Item = AnyErrorCollection>,
    {
        for collection in collections {
            self.absorb_any(collection);
        }
    }

    pub fn absorb_any(&mut self, other: AnyErrorCollection) {
        self.0.extend(other.0);
    }

    pub fn absorb<E>(&mut self, collection: ErrorCollection<E>)
    where
        E: DiagnosticWithLocation,
    {
        for err in collection {
            self.0.push(err);
        }
    }

    pub fn merge(&mut self, mut other: Self) {
        self.0.append(&mut other.0);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl Diagnostic for AnyErrorCollection {
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        if self.0.is_empty() {
            None
        } else {
            let iter = self.0.iter().map(|err| err.as_ref() as &dyn Diagnostic);
            Some(Box::new(iter))
        }
    }
}

impl fmt::Debug for AnyErrorCollection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "Empty Error Collection");
        }

        let handler = GraphicalReportHandler::new_themed(miette::GraphicalTheme::unicode());

        for err in &self.0 {
            handler.render_report(f, err.as_ref())?;
            writeln!(f)?;
        }

        Ok(())
    }
}

impl<E> From<ErrorCollection<E>> for AnyErrorCollection
where
    E: DiagnosticWithLocation,
{
    fn from(collection: ErrorCollection<E>) -> Self {
        let mut any = Self::new();
        any.absorb(collection);
        any
    }
}

impl fmt::Display for AnyErrorCollection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Compilation failed with {} error{}",
            self.0.len(),
            if self.0.len() == 1 { "" } else { "s" }
        )
    }
}

#[macro_export]
macro_rules! impl_diagnostic_with_location {
    ($t:ty, { $($variant:path),* $(,)? }) => {
        impl $crate::error::ErrorWithLocation for $t {
            fn location(&self) -> $crate::spanned::Location {
                match self {
                    $($variant { loc, .. } => *loc,)*
                }
            }
        }
    };
}
