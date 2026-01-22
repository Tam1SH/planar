use miette::{NamedSource, SourceSpan};

use crate::{
    module_loader::Source,
    spanned::{FileId, Location},
};
use std::collections::BTreeMap;

#[derive(Default, Debug, Clone)]
pub struct SourceRegistry {
    pub files: BTreeMap<FileId, Source>,
    next_id: u32,
}

impl SourceRegistry {
    pub fn add_with_id(&mut self, source: Source, id: FileId) {
        self.files.insert(id, source);
    }

    pub fn add(&mut self, source: Source) -> (FileId, &Source) {
        let id = FileId(self.next_id);
        self.next_id += 1;
        self.files.insert(id, source);
        (id, &self.files[&id])
    }

    pub fn get(&self, id: FileId) -> Option<&Source> {
        self.files.get(&id)
    }

    pub fn get_source_and_span(&self, loc: Location) -> (NamedSource<String>, SourceSpan) {
        let source = self.get(loc.file_id).expect("Invalid file_id");
        let named_source = NamedSource::new(source.origin.clone(), source.content.clone());
        let span = SourceSpan::new(loc.span.start.into(), loc.span.end - loc.span.start);
        (named_source, span)
    }
}
