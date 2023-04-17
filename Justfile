lint:
    cargo fmt
    cargo clippy --all-features --all-targets

sdk:
    cp src/sdk/grammar.rs src/sdk/grammar.rs.old
    cargo run sdk regen.grammar -t rust-self -o src/sdk/grammar.rs.tmp
    mv src/sdk/grammar.rs.tmp src/sdk/grammar.rs


docs:
    mkdir -p target/docs
    cargo run html regen.grammar -o target/docs/test.html
    cargo run html regen.grammar -t docs/regen_syntax.html.pp -o docs/regen_syntax.html