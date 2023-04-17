# regen-lang
Regen is a language that defines languages. The `regen` compiler generates a parser for the language defined with the Regen language in supported general-purpose programming languages:
- Rust
- TypeScript (TODO)
- Python (maybe)
- C++ (maybe)

Regen is suitable for small application-specific grammars. It is meant to replace `parse(cmd.split(" "))` with a more robust and easy-to-use parser. It is not meant to replace full-fledged parsers for existing programming languages, as those are more optimized and have more features like error recovery and better error messages.

## Features
- Regen defines LL (Left-to-right, Leftmost derivation) grammars.
- Lexer/Tokenizer parses tokens with regular expressions (basically DFA)
- Semantic annotations are built-in to the Abstract Syntax Tree.
- The Parse Tree nodes can be hooked with custom code to directly generate application-specific objects without the need for a separate pass.
- The generated parser API is type-safe (if the target language supports it).
- Syntax coloring to HTML, with optional classname mappings so you can use any existing theme.

## Syntax
[Here](https://regen.pistonite.org/regen_syntax) is the grammar of Regen, defined in Regen, and highlighted with the generated parser with a [Prism.js](https://prismjs.com/) theme.

## Documentation
TODO...