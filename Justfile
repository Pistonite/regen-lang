sdk:
    cp src/sdk/generated.rs src/sdk/generated.rs.old
    cargo run sdk regen.regen.txt -t rust-self -o src/sdk/generated.rs.tmp
    mv src/sdk/generated.rs.tmp src/sdk/generated.rs


docs:
    mkdir -p target/docs
    cargo run html regen.regen.txt -o target/docs/test.html
    cargo run html regen.regen.txt -t docs/regen_syntax.html.pp -o docs/regen_syntax.html