import math_go_brrr_py

print("Hi")


def brrr(f):
    import inspect, ast, ast2json, json

    code = inspect.getsource(f)
    tree = ast.parse(code)
    js = ast2json.ast2json(tree)
    ast_str = json.dumps(js, indent=2)
    math_go_brrr_py.take_source(ast_str)


@brrr
def foo(a, b):
    return a
