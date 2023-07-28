set windows-powershell:=true

lint:
    cargo fmt
    cargo clippy --all-features --all-targets

sdk:
    cp src/grammar.rs src/grammar.rs.old
    cargo run emit regen.grammar -t rust-self -o src/grammar.rs

docs:
    txtpp docs

release: lint docs
    cargo test
    cargo package
