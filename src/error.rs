use std::fmt::Debug;

/// Result type containing the custom error enum
pub type Result<T> = std::result::Result<T, Error>;

/// Error type containing all error cases with extra metadata
pub enum Error {
    /// An io operation failed
    FileSystem(std::io::Error),
    /// Parsing to Typescript failed
    Ts(String),
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::FileSystem(e) => write!(f, "FileSystem: {}", e),
            Error::Ts(e) => write!(f, "Ts: {}", e),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::FileSystem(e)
    }
}
