from math_go_brrr import FunctionGoneBrrr
from types import FunctionType
from collections.abc import Iterable


def assert_compatible(*args, f: FunctionGoneBrrr):
    assert not isinstance(f, FunctionType)
    assert callable(f)
    assert f(*args) == f.original_func(*args), f"{f(*args)} != {f.original_func(*args)}"


def assert_compatible_for_all(cases: Iterable[tuple], f: FunctionGoneBrrr):
    for args in cases:
        assert_compatible(*args, f=f)


def all_variations_3(*args) -> Iterable[tuple]:
    for a in args:
        for b in args:
            for c in args:
                yield (a, b, c)
