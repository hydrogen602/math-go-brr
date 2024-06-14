# JIT Python function to LLVM IR for fast numeric operations POC

This is a simple POC to show the concept of taking a python, analyzing its AST, and generating LLVM IR code that does what the python code does, and then making a callable with LLVM's JIT feature all using only a decorator.

The idea is that when a function is just basic math, there's no real need to deal with the overhead of python and its objects, and so instead we can translate it to basic LLVM IR `add` and `mul` instructions that have almost no overhead.

Now Cyton and maturin/PyO3 already do this, but the idea is to not require any compilation step or make the user interact with anything that isn't python, so that it is near seamless to the user (well there is always the restriction that llvm is statically typed, plus any such function can only contain integer / float operations).

---

```python
@brrr
def bar(a, b):
    return a + b
```

Here, `brrr` takes `bar`, gets and parses its source, and then generates an llvm function that does the `add` instructions on two i64s, and then creates a python-callable to replace `bar` that calls the llvm JIT engine.

---

```python
@brrr
def foo(a: int) -> int:
    b = 0
    i = 0
    while i < a:
        b = b + i
        i = i + 1

    return b


start = time()
llvm_out = foo(10_000_000)
print(f"LLVM JIT Compiled |", time() - start)

# foo.original_func is our plain-old Python function
start = time()
python_out = foo.original_func(10_000_000)
print(f"Regular Python    |", time() - start)
```
Outputs: (On an M1Pro MacBookPro)
```
LLVM JIT Compiled | 0.026205062866210938
Regular Python    | 0.3830869197845459
```
