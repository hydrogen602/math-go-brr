from types import FunctionType
from math_go_brrr_py import brrr


def test_one():
    @brrr
    def foo(a: int, b: int) -> int:
        return a

    assert not isinstance(foo, FunctionType)
    assert foo(5, 8) == 5

    @brrr
    def foo(a: int, b: int) -> int:
        return b

    assert not isinstance(foo, FunctionType)
    assert foo(5, 8) == 8


def test_two():
    @brrr
    def bar(a: int, b: int) -> int:
        return a - b

    assert not isinstance(bar, FunctionType)
    assert bar(5, 8) == -3

    @brrr
    def bar(a: int, b: int) -> int:
        return a + b

    assert not isinstance(bar, FunctionType)
    assert bar(5, 8) == 13


def test_more():
    @brrr
    def bar(a: int, b: int) -> int:
        return a + b - b - b

    assert not isinstance(bar, FunctionType)
    assert bar(5, 8) == -3
