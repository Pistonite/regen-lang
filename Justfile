sdk:
    python gen_sdk.py > src/sdk_generated.rs
    cp src/sdk/generated.rs src/sdk/generated.rs.old
    cargo run sdk 1 > src/sdk/generated.rs.tmp
    mv src/sdk/generated.rs.tmp src/sdk/generated.rs


docs:
    mkdir -p docs/build
    cargo run tokenize regen.regen.txt --header docs/test.header.html --footer docs/test.footer.html -o docs/build/tokenize.html
    cargo run highlight regen.regen.txt --header docs/test.header.html --footer docs/test.footer.html -o docs/build/highlight.html