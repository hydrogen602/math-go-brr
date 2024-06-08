
```sh
maturin develop
```

---

If you need to have llvm@17 first in your PATH, run:
  echo 'export PATH="/opt/homebrew/opt/llvm@17/bin:$PATH"' >> ~/.zshrc

For compilers to find llvm@17 you may need to set:
  export LDFLAGS="-L/opt/homebrew/opt/llvm@17/lib"
  export CPPFLAGS="-I/opt/homebrew/opt/llvm@17/include"

Run before cargo run:
╰─$ export LLVM_SYS_170_PREFIX='/opt/homebrew/opt/llvm@17/'

Flow
Python AST -> ast2json -> JSON -> Rust serde (permissive) -> Rust AST (strict) -> LLVM codegen