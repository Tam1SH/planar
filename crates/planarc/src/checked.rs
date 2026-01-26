use tracing::{debug, error, instrument, trace};

use crate::error::{AnyErrorCollection, DiagnosticWithLocation, ErrorCollection};

pub struct Checked<T, E = AnyErrorCollection> {
    value: T,
    errors: E,
}

impl<T, E: Default> Checked<T, E> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            errors: E::default(),
        }
    }
}

impl<T, E> Checked<T, E> {
    pub fn into_parts(self) -> (T, E) {
        (self.value, self.errors)
    }

    pub fn with_errors(value: T, errors: E) -> Self {
        Self { value, errors }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Checked<U, E> {
        Checked {
            value: f(self.value),
            errors: self.errors,
        }
    }

    pub fn sink(self, collector: &mut E) -> T
    where
        E: MergeableErrors,
    {
        collector.merge(self.errors);
        self.value
    }

    pub fn accumulate<U>(mut self, other: Checked<U, E>) -> Checked<(T, U), E>
    where
        E: MergeableErrors,
    {
        self.errors.merge(other.errors);
        Checked {
            value: (self.value, other.value),
            errors: self.errors,
        }
    }
}

pub trait MergeableErrors {
    fn merge(&mut self, other: Self);
}

impl MergeableErrors for AnyErrorCollection {
    #[instrument(skip_all)]
    fn merge(&mut self, other: Self) {
        if !other.0.is_empty() {
            error!(
                added = other.0.len(),
                total_before = self.0.len(),
                "Merging errors into AnyErrorCollection"
            );

            for err in &other.0 {
                trace!(error = %err, "Error being merged");
            }
            self.0.extend(other.0);
        }
    }
}

impl<E: DiagnosticWithLocation> MergeableErrors for ErrorCollection<E> {
    #[instrument(skip_all)]
    fn merge(&mut self, other: Self) {
        if !other.0.is_empty() {
            error!(
                added = other.0.len(),
                total_before = self.0.len(),
                "Merging generic errors"
            );
            
            for err in &other.0 {
                trace!(error = %err, "Error being merged");
            }
            
            self.0.extend(other.0);
        }
    }
}

pub trait CheckedIteratorExt<T, E>: Sized {
    fn collect_checked(self) -> Checked<Vec<T>, E>;
}

impl<I, T, E> CheckedIteratorExt<T, E> for I
where
    I: Iterator<Item = Checked<T, E>>,
    E: Default + MergeableErrors,
{
    fn collect_checked(self) -> Checked<Vec<T>, E> {
        let mut errors = E::default();
        let value = self
            .map(|checked| {
                errors.merge(checked.errors);
                checked.value
            })
            .collect();

        Checked { value, errors }
    }
}
