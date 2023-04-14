TARGET = "src/sdk_generated.rs"
import os

if __name__ == "__main__":
    with open(TARGET, "w", encoding="utf-8") as f:
        f.write("// Generated SDK code to be emitted\n")
        for sdk in os.listdir("sdk"):
            sdk_name = sdk.upper().replace(".", "_")
            with open(os.path.join("sdk", sdk), "r", encoding="utf-8") as sdk_file:
                sdk_content = sdk_file.read()
            sdk_content = "\n".join([ f"/*{sdk_name}*/{line}" for line in sdk_content.splitlines() if line.strip() ])
            
            f.write(f"pub const {sdk_name}: &str = r#######\"\n{sdk_content}\"#######;\n")
            