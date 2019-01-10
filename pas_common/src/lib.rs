use {
    std::{
        ops::Deref,
    },
    backtrace::Backtrace,
};

#[derive(Clone, Debug)]
pub struct TracedError<T> {
    pub err: T,
    pub bt: Backtrace,
}

impl<T> TracedError<T> {
    pub fn trace(err: T) -> Self {
        const SKIP_FRAMES: usize = 5;

        let mut frames: Vec<_> = Backtrace::new().into();
        frames.rotate_left(SKIP_FRAMES);
        frames.truncate(frames.len() - SKIP_FRAMES);

        Self {
            err,
            bt: frames.into(),
        }
    }

    pub fn chain<TNext: From<T>>(self) -> TracedError<TNext> {
        TracedError {
            err: self.err.into(),
            bt: self.bt
        }
    }
}

impl<T> Deref for TracedError<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.err
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuildOptions {
    pub case_sensitive: bool,
}