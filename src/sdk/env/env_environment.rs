use crate::sdk::{EnvImpl, Environment, Error, Mode, RootParser, TokenStream, TokenType};

use super::State;

impl<T, A, C> Environment for EnvImpl<T, A, C>
where
    T: TokenType,
    Self: RootParser<T = T, A = A, C = C>,
{
    fn tokenize(&mut self) {
        if self.state == State::Init {
            self.tokens = Self::do_tokenize(&self.src, &mut self.ctx);
            self.state = State::Tokenized;
        }
    }

    fn parse_asts(&mut self) {
        if self.state >= State::ASTParsed {
            return;
        }
        self.tokenize();

        let mut ts = TokenStream::new(&self.tokens, self.stack_size);
        let mut last_failed = false;
        let mut asts = Vec::new();

        loop {
            match Self::parse_ast_root(&mut ts, &mut self.ctx) {
                Some(ast) => {
                    last_failed = false;
                    asts.push(ast);
                    if let Mode::One = self.mode {
                        break;
                    }
                    if ts.is_exhausted() {
                        break;
                    }
                }
                None => {
                    if let Some(token) = ts.consume() {
                        if !last_failed {
                            last_failed = true;
                            if let Some(err_token) = ts.get_guess_err_token() {
                                self.ctx.err.push(Error::from_token_without_help(
                                    err_token,
                                    "Syntax error near this location".to_owned(),
                                ));
                            } else {
                                self.ctx.err.push(Error::from_token_without_help(
                                    token,
                                    "Syntax error".to_owned(),
                                ));
                            }
                        }
                    } else {
                        break;
                    }
                }
            }
        }
        self.state = State::ASTParsed;
        self.asts = asts;
    }

    fn parse_pts_then<'s, 'p, P, F, R>(&'s mut self, consumer: F) -> Result<R, &[Error]>
    where
        F: FnOnce(
            Vec<<Self as RootParser>::P<'p>>,
            &mut <Self as RootParser>::C,
        ) -> Result<R, Vec<Error>>,
        's: 'p,
    {
        self.parse_asts();
        if !self.ctx.err.is_empty() {
            return Err(&self.ctx.err);
        }

        let mut pts = Vec::new();

        for ast in &self.asts {
            let pt = Self::parse_pt_root(ast, &mut self.ctx);
            pts.push(pt);
        }
        if !self.ctx.err.is_empty() {
            return Err(&self.ctx.err);
        }
        let result = consumer(pts, self.ctx.val.as_mut());
        //self.asts.append(&mut asts);
        match result {
            Ok(r) => Ok(r),
            Err(mut errs) => {
                self.ctx.err.append(&mut errs);
                Err(&self.ctx.err)
            }
        }
    }
}
