from types import FunctionType
from math_go_brrr import brrr


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


def test_const_0():
    @brrr
    def foo() -> int:
        return 0

    assert foo() == 0


def test_const_pos():
    @brrr
    def foo() -> int:
        return 42

    assert foo() == 42


def test_const_neg():
    @brrr
    def foo() -> int:
        return -42

    assert foo() == -42


def test_const_more_than_i32():
    @brrr
    def foo() -> int:
        return 360_777_252_864  # 42 * 2 << 32

    assert foo() == 360_777_252_864


def test_const_more_than_i32_neg():
    @brrr
    def foo() -> int:
        return -360_777_252_864

    assert foo() == -360_777_252_864


def test_const_u32_vs_i32():
    @brrr
    def foo() -> int:
        return 9223372036854775807

    assert foo() == 9223372036854775807

    @brrr
    def foo() -> int:
        return -9_223_372_036_854_775_807

    assert foo() == -9_223_372_036_854_775_807
