use std::fmt;

#[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct LineCol {
    /// One-indexed line.
    pub line: usize,
    /// Zero-indexed column.
    pub col: usize,
}

impl Default for LineCol {
    fn default() -> Self {
        Self { line: 1, col: 0 }
    }
}

impl fmt::Display for LineCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
