from math_go_brrr import brrr
from time import time
from types import FunctionType


def foo(a: int) -> int:
    b = 0
    i = 0
    while i < a:
        b = b + i
        i = i + 1

    return b


if __name__ == "__main__":
    for lvl in ["none", "less", "default", "aggressive"]:
        foo_compiled = brrr(optimization=lvl)(foo)

        start = time()
        llvm_out = foo_compiled(10_000_000)
        print(f"LLVM opt = {lvl:<10} |", time() - start)

    start = time()
    python_out = foo(10_000_000)
    print(f"Regular Python        |", time() - start)
