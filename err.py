from math_go_brrr import brrr
from types import TracebackType
import sys

# try:

try:

    @brrr
    def foo(a: int) -> int:
        b = a == 3
        b = a
        return a

except TypeError as e:
    print(e)
    print(type(e))

    raise


# except TypeError as e:
#     traceback = e.__traceback__
#     t = TracebackType(
#         tb_next=None, tb_frame=traceback.tb_frame, tb_lasti=8, tb_lineno=8
#     )
#     raise e.with_traceback(t)
