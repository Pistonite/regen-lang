sdk:
    cp src/sdk/generated.rs src/sdk/generated.rs.old
    cargo run sdk 1 > src/sdk/generated.rs.tmp
    mv src/sdk/generated.rs.tmp src/sdk/generated.rs


docs:
    mkdir -p target/docs
    cargo run html regen.regen.txt -o target/docs/test.html