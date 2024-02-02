#![allow(dead_code)]


use crate::analyze::lex::line::Line;


pub struct SourceFile {
    pub lines: Vec<Line>,
}

impl SourceFile {
    pub fn len(&self) -> usize {
        self.lines.len()
    }
    pub fn get(&self, index: usize) -> Option<&Line> {
        self.lines.get(index)
    }
}

impl From<Vec<Line>> for SourceFile {
    fn from(lines: Vec<Line>) -> Self {
        Self { lines }
    }
}
