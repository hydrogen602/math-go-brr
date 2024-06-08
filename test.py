from types import FunctionType
from math_go_brrr import brrr

# def brrr(f):
#     import inspect, ast, ast2json, json, math_go_brrr_py

#     code = inspect.getsource(f)
#     tree = ast.parse(code)
#     js = ast2json.ast2json(tree)
#     ast_str = json.dumps(js, indent=2)
#     return math_go_brrr_py.take_source(ast_str)


@brrr
def foo(a: int, b: int) -> int:
    return a


assert not isinstance(foo, FunctionType)
assert foo(5, 8) == 5


@brrr
def bar(a: int, b: int) -> int:
    return a - b


assert not isinstance(bar, FunctionType)
assert bar(5, 8) == -3


print("All tests passed!")
