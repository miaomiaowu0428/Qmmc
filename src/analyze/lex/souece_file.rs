use crate::analyze::lex::line::Line;

pub struct SourceFile {
    pub lines: Vec<Line>
}

impl From<Vec<Line>> for SourceFile {
    fn from(lines: Vec<Line>) -> Self {
        Self { lines }
    }
}