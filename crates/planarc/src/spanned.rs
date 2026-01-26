use derive_more::Display;
use rkyv::{Archive, Deserialize, Serialize};
use std::fmt::Debug;

#[derive(
    Default,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    Archive,
    Serialize,
    Deserialize,
    Display,
)]
#[rkyv(derive(Debug, PartialEq, Eq, PartialOrd, Ord))]
pub struct FileId(pub usize);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize, Display)]
#[rkyv(derive(Debug))]
#[display("{file_id} @ {span}")]
pub struct Location {
    pub file_id: FileId,
    pub span: Span,
}

impl Location {
    pub fn new(file_id: FileId, span: Span) -> Self {
        Self { file_id, span }
    }
}

#[derive(Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub loc: Location,
}

impl<T> Spanned<T> {
    pub fn new(value: T, loc: Location) -> Self {
        Self { value, loc }
    }

    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            value: f(self.value),
            loc: self.loc,
        }
    }


    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            loc: self.loc,
        }
    }

    pub fn and_then<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> Spanned<U>,
    {
        f(self.value)
    }

    pub fn at(value: T, file_id: FileId, span: Span) -> Self {
        Self {
            value,
            loc: Location::new(file_id, span),
        }
    }

}

pub trait ToSpanned: Sized {
    fn spanned(self, loc: Location) -> Spanned<Self> {
        Spanned::new(self, loc)
    }

    fn spanned_inherit<U>(self, other: &Spanned<U>) -> Spanned<Self> {
        Spanned {
            value: self,
            loc: other.loc,
        }
    }

    fn spanned_at(self, file_id: FileId, span: Span) -> Spanned<Self> {
        Spanned::at(self, file_id, span)
    }

}

impl<T> ToSpanned for T {}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?} @ {:?}", self.value, self.loc.span)
        } else {
            write!(f, "{:?} @ {:?}", self.value, self.loc.span)
        }
    }
}

impl<T> std::fmt::Debug for ArchivedSpanned<T>
where
    T: rkyv::Archive,
    T::Archived: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?} @ {:?}", self.value, self.loc.span)
        } else {
            write!(f, "{:?} @ {:?}", self.value, self.loc.span)
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize, Display)]
#[rkyv(derive(Debug))]
#[display("{line}:{col} ({start}..{end})")]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub col: usize,
    pub line_end: usize,
    pub col_end: usize,
}

impl Span {
    pub fn new(
        start: usize,
        end: usize,
        line: usize,
        col: usize,
        line_end: usize,
        col_end: usize,
    ) -> Self {
        Self {
            start,
            end,
            line,
            col,
            col_end,
            line_end,
        }
    }

    pub fn from_range(range: std::ops::Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
            ..Default::default()
        }
    }
}

impl From<Location> for miette::SourceSpan {
    fn from(loc: Location) -> Self {
        (loc.span.start..loc.span.end).into()
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self::from_range(value)
    }
}
