use crate::{
    module_loader::Source,
    spanned::{FileId, Location, Span, Spanned},
};
use type_sitter::Node;

pub struct Ctx<'a> {
    pub source: &'a Source,
    pub file_id: FileId,
}

impl<'a> Ctx<'a> {
    pub fn new(source: &'a Source, file_id: FileId) -> Self {
        Self { source, file_id }
    }

    pub fn spanned<T>(&self, node: &impl Node<'a>, value: T) -> Spanned<T> {
        let range = node.range();

        let span = Span::new(
            range.start_byte,
            range.end_byte,
            (range.start_point.row + 1) as u32,
            (range.start_point.column + 1) as u32,
            range.end_point.row as u32,
            range.end_point.column as u32,
        );

        Spanned {
            value,
            loc: Location {
                file_id: self.file_id,
                span,
            },
        }
    }

    pub fn location(&self, node: &impl Node<'a>) -> Location {
        self.spanned(node, ()).loc
    }

    pub fn text(&self, node: &impl Node<'a>) -> String {
        node.utf8_text(self.source.content.as_bytes())
            .unwrap_or("")
            .to_string()
    }
}
