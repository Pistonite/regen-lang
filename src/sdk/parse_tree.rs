use super::{Error, TokenBlocks, TokenType};

/// Trait used for converting AST to Parse Tree
///
/// This trait is implemented by the root of the generated AST. Use this to generate a Parse Tree
/// from the AST
pub trait CreateParseTree {
    type T: TokenType;
    type C;
    type P<'p>
    where
        Self: 'p;
    /// Create parse tree
    ///
    /// Creates a Parse Tree root from the AST reference. Note that Parse Tree nodes contain references
    /// to the AST, so the AST must be kept in memory until the Parse Tree is dropped.

    fn parse_pt(&self, ctx_obj: Box<Self::C>) -> ParseTreeResult<Self::P<'_>, Self::C> {
        match self.parse_pt_with_semantic(TokenBlocks::new(""), ctx_obj) {
            ParseTreeResultSemantic::Ok { pt, ctx, .. } => ParseTreeResult::Ok { pt, ctx },
            ParseTreeResultSemantic::Err { pt, ctx, err, .. } => {
                ParseTreeResult::Err { pt, ctx, err }
            }
        }
    }

    fn parse_pt_with_semantic(
        &self,
        tbs: TokenBlocks<Self::T>,
        ctx_obj: Box<Self::C>,
    ) -> ParseTreeResultSemantic<Self::P<'_>, Self::C, Self::T> {
        let mut ctx = ParseTreeContext {
            tbs,
            err: vec![],
            val: ctx_obj,
        };
        let pt = self.parse_pt_with_context_internal(&mut ctx);
        if ctx.err.is_empty() {
            ParseTreeResultSemantic::Ok {
                pt,
                ctx: ctx.val,
                tbs: ctx.tbs,
            }
        } else {
            ParseTreeResultSemantic::Err {
                pt,
                ctx: ctx.val,
                tbs: ctx.tbs,
                err: ctx.err,
            }
        }
    }

    fn parse_pt_with_context_internal(
        &self,
        ctx: &mut ParseTreeContext<Self::C, Self::T>,
    ) -> Self::P<'_>;
}

pub enum ParseTreeResult<P, C> {
    Ok { pt: P, ctx: Box<C> },
    Err { pt: P, ctx: Box<C>, err: Vec<Error> },
}
pub enum ParseTreeResultSemantic<P, C, T>
where
    T: TokenType,
{
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
    },
}

/// Struct used internally to store the context while parsing the parse tree.
#[derive(Debug)]
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
