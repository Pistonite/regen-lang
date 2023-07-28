use super::{TokenType, TokenBlocks, Error};

/// Trait used for converting AST to Parse Tree
///
/// This trait is implemented by the root of the generated AST. Use this to generate a Parse Tree
/// from the AST
pub trait CreateParseTree {
    type T: TokenType;
    type C;
    type P<'p> where Self: 'p;
    /// Create parse tree
    ///
    /// Creates a Parse Tree root from the AST reference. Note that Parse Tree nodes contain references
    /// to the AST, so the AST must be kept in memory until the Parse Tree is dropped.

    fn parse_pt<'a>(
        &'a self, 
        ctx_obj: Box<Self::C>,
    ) -> ParseTreeResult<Self::P<'a>, Self::C, Self::T> {
        let mut tbs = TokenBlocks::new("");
        self.parse_pt_with_semantic(&mut tbs, ctx_obj)
    }

    fn parse_pt_with_semantic<'a>(
        &'a self, 
        tbs: &mut TokenBlocks<Self::T>, 
        ctx_obj: Box<Self::C>,
    ) -> ParseTreeResult<Self::P<'a>, Self::C, Self::T> {
        let mut tbs = TokenBlocks::new("");
        let mut ctx = ParseTreeContext {
            tbs: TokenBlocks::new(""),
            err: vec![],
            val: ctx_obj,
        };
        let pt = self.parse_pt_with_context(&mut ctx);
        if ctx.err.is_empty() {
            Ok((pt, ctx.val))
        } else {
            Err((ctx.err, ctx.val))
        }
    }

    fn parse_pt_with_context_interal<'a>(&'a self, ctx: &mut ParseTreeContext<Self::C, Self::T>) -> Self::P<'a>;
}

pub enum ParseTreeResult<P, C, T> where T: TokenType {
    Ok {
        pt: P,
        ctx: Box<C>,
        tbs: TokenBlocks<T>,
    },
    Err {
        pt: P,
        ctx: Box<C>,
        tbs: TokenBlocks<T>,
        err: Vec<Error>,
    }
}

/// Struct used internally to store the context while parsing the parse tree.
#[derive(Default)]
pub struct ParseTreeContext<C, T>
where
    T: TokenType,
{
    /// The semantic information stored as token blocks
    pub tbs: TokenBlocks<T>,
    /// The errors encountered during parsing
    pub err: Vec<Error>,
    /// The application-specific context object.
    ///
    /// For Regen, this is [`crate::core::Language`].
    pub val: Box<C>,
}
