use html_escape;
use super::Token;

pub trait ISemantics: From<Self::Tokens> {
    type Tokens: Clone;
    fn to_html_class(&self) -> String;
}

/// A span is part of the input source code marked with a semantic type.
pub struct Span<S>{
    /// The semantic type of the span
    pub semantic_type: Option<S>,
    /// The text of the span
    pub text: String,
}

impl<S: ISemantics> Span<S>{
    /// Convert the span to an html string, with proper escapes
    pub fn to_html_default(&self) -> String {
        self.to_html(|s| s.to_html_class())
    }
    pub fn to_html<F>(&self, sematic_mapping: F) -> String
    where F: Fn(&S) -> String
     {
        let text = html_escape::encode_text(&self.text).to_string();
        match &self.semantic_type {
            None => text,
            Some(semantic_type) => {
                format!(r#"<span class="{cls}">{text}</span>"#, cls=sematic_mapping(semantic_type))
            }
        }
    }
}

/// Semantic block used internally by SemInfo to mark a range of text with a semantic
struct SemBlock<S> where S: ISemantics{
    /// Semantic type
    semantic_type: S,
    /// Token absolute position - start and end
    pos: (usize, usize),
}

/// Semantic information that is stored and updated during the parsing process
pub struct SemInfoImpl<S> where S: ISemantics{
    blocks: Vec<SemBlock<S>>,
}

impl<S: ISemantics + Clone> SemInfoImpl<S>{
    /// Create a new SemInfo
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    /// Insert all tokens and mark them with the associated token semantic
    pub fn insert_all(&mut self, tokens: &[Token<S::Tokens>]) {
        for t in tokens {
            self.blocks.push(SemBlock {
                semantic_type: t.token_type.clone().into(),
                pos: t.pos,
            });
        }
        self.blocks.sort_by(|a, b| {
            a.pos.0.cmp(&b.pos.0)
        })
    }

    /// Set the semantic of a token
    pub fn set(&mut self, token: &Token<S::Tokens>, semantic_type: S) {
        let result = self.blocks.binary_search_by(|probe| {
            probe.pos.0.cmp(&token.pos.0)
        });
        match result {
            Ok(index) => {
                
                self.blocks[index].semantic_type = semantic_type;
            },
            Err(index) => {
                self.blocks.insert(index, SemBlock {
                    semantic_type,
                    pos: token.pos,
                });
            }
        }
    }

    /// Break down input content into spans marked with semantics
    pub fn get_spans(&self, content: &str) -> Vec<Span<S>>
     {
        let mut code_blocks = Vec::new();
        let mut cur = 0;
        for semantic_token in &self.blocks {
            let (start, end) = semantic_token.pos;
            if start > cur {
                code_blocks.push(Span {
                    semantic_type: None,
                    text: content[cur..start].to_owned(),
                });
            }
            cur = cur.max(start);
            if end > cur {
                code_blocks.push(Span {
                    semantic_type: Some(semantic_token.semantic_type.clone()),
                    text: content[cur..end].to_owned(),
                });
                cur = end;
            }
        }
        if cur < content.len() {
            code_blocks.push(Span {
                semantic_type: None,
                text: content[cur..].to_owned(),
            });
        }
        code_blocks
    }

    pub fn get_html_default(&self, content: &str) -> String {
        self.get_html(content, |s| s.to_html_class())
    }

    pub fn get_html<F>(&self, content: &str, semantic_mapping: F) -> String 
    where F: Fn(&S) -> String
    {
        let mut html = String::new();
        for span in self.get_spans(content) {
            html.push_str(&span.to_html(&semantic_mapping));
        }
        html
    }
}