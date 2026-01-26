use miette::{NamedSource, SourceSpan};

use crate::{
    module_loader::Source,
    spanned::{FileId, Location},
};
use std::{collections::BTreeMap, sync::Arc};

pub type MietteSource = Arc<NamedSource<Arc<String>>>;

#[derive(Default, Debug, Clone)]
pub struct SourceRegistry {
    pub files: BTreeMap<FileId, MietteSource>,
}

impl SourceRegistry {
    pub fn add_with_id(&mut self, source: MietteSource, id: FileId) {
        self.files.insert(id, source);
    }

    pub fn get(&self, id: FileId) -> Option<&MietteSource> {
        self.files.get(&id)
    }

    pub fn get_source_and_span(&self, loc: Location) -> (MietteSource, SourceSpan) {
        let source = self.files.get(&loc.file_id).unwrap_or_else(|| {
            panic!(
                "Invalid file_id: Registry out of sync with AST/Locations, got: {:?}",
                loc
            )
        });

        let named_source = Arc::clone(source);

        let span = SourceSpan::new(loc.span.start.into(), loc.span.end - loc.span.start);

        (named_source, span)
    }
}
