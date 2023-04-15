pub mod generated;
pub use generated::{
    tokenize
};

pub mod util;

pub mod gen;
#[macro_export]
macro_rules! tree_cast {
    ($enum_type:ident :: $enum_variant:ident $var:expr )=> {
        match $var {
            $enum_type::$enum_variant(x) => x,
            _ => panic!("Unable to tree_cast: Expected {}, got {:?}", stringify!($match_type), $var)
        }
    };
}
#[macro_export]
macro_rules! err {
    ($token_ref:expr, $str:expr) => {
        RegenError {
            msg: $str,
            pos: $token_ref.pos,
        }
    };
}


// Parser
#[derive(Debug)]
pub struct RegenError {
    pub msg: String,
    pub pos: (usize, usize),
}

impl RegenError {
    pub fn pretty_print(&self, source: &str, context: usize) {
        let (l, c) = self.to_line_col(source);
        let m = &self.msg;
        eprintln!("Error at line {l}, column {c}: {m}");

        let start = if l > context { l - context } else { 1 };
        for (i, line_str) in source.lines().skip(start-1).take(context*2+1).enumerate() {
            let current = i+start;
            eprintln!("{current:3} | {line_str}");
            if current == l {
                eprintln!("{:3} | {:>width$}{t}", "", "", width = c-1, t= "^".repeat(self.pos.1 - self.pos.0));
            }
        }
    }

    fn to_line_col(&self, content: &str) -> (usize, usize) {
        let (start, _) = self.pos;
        let mut cur = 0;
        let mut l = 1;
        for line in content.split('\n') {
            let line_len = line.len() + 1;
            if cur + line_len > start {
                return (l, start - cur + 1);
            }
            cur += line_len;
            l += 1;
        }
        (l, 0)
    }

}
