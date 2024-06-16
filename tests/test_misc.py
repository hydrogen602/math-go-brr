from math_go_brrr import brrr
from .util import assert_compatible_for_all


def test_plus_equals():
    @brrr(dump_ast_json=True)
    def foo(a: int, b: int) -> int:
        a += b
        return a

    assert_compatible_for_all([(1, 2), (3, 4), (5, 6)], f=foo)

    @brrr
    def foo(a: int, b: int) -> int:
        a -= b
        return a

    assert_compatible_for_all([(1, 2), (3, 4), (5, 6)], f=foo)


def test_mul():
    @brrr
    def foo(a: int, b: int) -> int:
        return a * b

    assert_compatible_for_all([(1, 2), (3, 4), (5, 6)], f=foo)
