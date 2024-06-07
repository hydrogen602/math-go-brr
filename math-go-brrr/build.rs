fn main() {
    // link /opt/homebrew/opt/zstd/lib
    println!("cargo:rustc-link-search=/opt/homebrew/opt/zstd/lib");
}
