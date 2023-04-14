

#[derive(Debug)]
pub struct ParseHook<H, P> {
    pub pt: P,
    pub val: Option<H>,
}

impl<H, P> ParseHook<H, P> {
    pub fn take_unchecked(&mut self) -> H {
        self.val.take().unwrap_or_else(||panic!("ParseHook::take() called on None. Make sure your parse hook function is not taking the value twice."))
    }
}