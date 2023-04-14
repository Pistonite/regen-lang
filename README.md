highlight_tokens: tokenize and return highlighted blocks. fastest, won't have semantic information or parsed object
returns: Vec<CodeBlock>
highlight_semantic_one: tokenize and return semantic-highlighted blocks with information from the AST. slower, will have semantic information from the AST
  one means it will stop after the first AST is generated, and discard the rest of the tokens
returns: Vec<CodeBlock>, Option<ASTTarget>
highlight_semantic: tokenize and return semantic-highlighted blocks with information from the AST. slower, will have semantic information from the AST
  this will keep generating ASTs until it has parsed the entire token stream
  if AST cannot be generated, it will skip one toke and try again
returns: Vec<CodeBlock>, Vec<ASTTarget>, Vec<ParserError>

parse_one: generate PT from the first AST. slowest, will have parsed object and accurate semantic-highlighted blocks
returns: Option<PTTarget>, Option<ASTTarget>, Vec<CodeBlock>, Vec<ParserError>
parse: generate PT from all ASTs. slowest, will have parsed object and accurate semantic-highlighted blocks
returns: Vec<PTTarget>, Vec<ASTTarget>, Vec<CodeBlock>, Vec<ParserError>