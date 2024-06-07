# JIT Python function to LLVM IR for fast numeric operations POC

This is a simple POC to show the concept of taking a python, analyzing its AST, and generating LLVM IR code that does what the python code does, and then making a callable with LLVM's JIT feature all using only a decorator.

The idea is that when a function is just basic math, there's no real need to deal with the overhead of python and its objects, and so instead we can translate it to basic LLVM IR `add` and `mul` instructions that have almost no overhead.

Now Cyton and maturin/PyO3 already do this, but the idea is to not require any compilation step or make the user interact with anything that isn't python, so that it is near seamless to the user (well there is always the restriction that llvm is statically typed, plus any such function can only contain integer / float operations).

