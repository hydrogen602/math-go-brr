"""
This script is a collection of samples for developing useful errors and tracebacks
"""

from math_go_brrr import brrr, generate
from types import TracebackType
import sys


idx = 0
if len(sys.argv) >= 2:
    idx = int(sys.argv[1])


if idx == 0:

    @brrr
    def foo(a: int) -> int:
        b = a == 3
        b = a
        return a

elif idx == 1:

    @brrr
    def foo(a: int) -> bool:
        return a

elif idx == 2:

    @brrr
    def foo(a: int) -> int:
        return a // 0

    foo(1)


# elif idx == 2:

#     @brrr
#     def foo(a: int) -> int:
#         return a
