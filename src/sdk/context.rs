use super::{
    Error,
    Token,
    TokenStream,
    SemInfoImpl,
    ISemantics
};

pub trait IASTTarget: Sized {
    type Tokens: Clone;
    type Semantics: ISemantics;
    fn parse_root(ts: &mut TokenStream<Self::Tokens>, si: &mut SemInfoImpl<Self::Semantics>) -> Option<Self>;
}

pub trait IPTTarget<'a, A, S> where S: ISemantics, A: IASTTarget {
    fn parse_root(ast: &'a A, si: &mut SemInfoImpl<S>, err: &mut Vec<Error>) -> Self ;
}

pub struct TokenizeResult<S> where S: ISemantics {
    pub si: SemInfoImpl<S>,
    pub tokens: Vec<Token<S::Tokens>>,
}

impl<S: ISemantics> TokenizeResult<S> {
    pub fn ast_one<A>(mut self, stack_size: usize) -> AbstractSyntaxTree<S, A> where A: IASTTarget<Semantics = S, Tokens = S::Tokens>{
        let mut ts = TokenStream::new(&self.tokens, stack_size);
        let mut asts = Vec::new();
        let ast = A::parse_root(&mut ts, &mut self.si);
        if let Some(ast) = ast {
            //ast.apply_semantic(&mut self.si);
            asts.push(ast);
        }
        AbstractSyntaxTree {
            si: self.si,
            err: vec![],
            tokens: self.tokens,
            asts
        }
    }
    pub fn ast_all<A>(self, stack_size: usize) -> Result<AbstractSyntaxTree<S, A>, Vec<Error>> where A: IASTTarget<Semantics = S, Tokens = S::Tokens>{
        let ctx = self.ast_all_unchecked(stack_size);
        if ctx.err.is_empty() {
            Ok(ctx)
        } else {
            Err(ctx.err)
        }
    }

    pub fn ast_all_unchecked<A>(mut self, stack_size: usize) -> AbstractSyntaxTree<S, A> where A: IASTTarget<Semantics = S, Tokens = S::Tokens> {
        let mut ts = TokenStream::new(&self.tokens, stack_size);
        let mut asts = Vec::new();
        let mut last_failed = false;
        let mut err = Vec::new();

        loop {
            match A::parse_root(&mut ts, &mut self.si) {
                Some(ast) => {
                    //ast.apply_semantic(&mut self.si);
                    last_failed = false;
                    asts.push(ast);
                    if ts.is_exhausted() {
                        break;
                    }
                },
                None => {
                    if let Some(token) = ts.consume() {
                        if !last_failed {
                            last_failed = true;
                            if let Some(err_token) = ts.get_guess_err_token() {
                                err.push(Error::from_token_without_help(err_token, "Syntax error near this location".to_owned()));
                            }else{
                                err.push(Error::from_token_without_help(token, "Syntax error".to_owned()));
                            }
                        }
                    } else {
                        break;
                    }
                }
            }
        }

        AbstractSyntaxTree {
            si: self.si,
            err,
            tokens: self.tokens,
            asts
        }
    }
}

pub struct AbstractSyntaxTree<S, A> where S: ISemantics {
    pub si: SemInfoImpl<S>,
    pub err: Vec<Error>,
    pub tokens: Vec<Token<S::Tokens>>,
    pub asts: Vec<A>,
}

impl<S: ISemantics, A: IASTTarget<Semantics = S, Tokens = S::Tokens>> AbstractSyntaxTree<S, A> {
    pub fn parse<'a, P>(&'a mut self) -> Result<Vec<P>, &'a[Error]> where P: IPTTarget<'a, A, S> {
        let pts = Self::do_parse_unchecked(&self.asts, &mut self.si, &mut self.err);
        
        if self.err.is_empty() {
            Ok(pts)
        } else {
            Err(&self.err)
        }
    }

    #[inline]
    pub fn parse_unchecked<'a, P>(&'a mut self) -> Vec<P> where P: IPTTarget<'a, A, S> {
        Self::do_parse_unchecked(&self.asts, &mut self.si, &mut self.err)
    }

    pub fn do_parse_unchecked<'a, P>(asts: &'a [A], si: &mut SemInfoImpl<S>, err: &mut Vec<Error>) -> Vec<P> where P: IPTTarget<'a, A, S> {
        asts.iter().map(|ast| {
            P::parse_root(ast, si, err)
        }).collect()
    }
    
}

