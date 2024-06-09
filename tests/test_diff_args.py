from math_go_brrr import brrr


def test_0():
    @brrr
    def foo() -> int:
        return 0

    assert foo() == 0


def test_1():
    @brrr
    def foo(a: int) -> int:
        return a

    assert foo(2) == 2


def test_2():
    @brrr
    def foo(a: int, b: int) -> int:
        return a + b

    assert foo(3, 4) == 7


def test_3():
    @brrr
    def foo(a: int, b: int, c: int) -> int:
        return a + b + c

    assert foo(3, 4, 5) == 12


def test_4():
    @brrr
    def foo(a: int, b: int, c: int, d: int) -> int:
        return a + b + c + d

    assert foo(3, 4, 5, 7) == 19


def test_5():
    @brrr
    def foo(a: int, b: int, c: int, d: int, e: int) -> int:
        return a + b + c + d + e

    assert foo(3, 4, 5, 7, 9) == 28


def test_6():
    @brrr
    def foo(a: int, b: int, c: int, d: int, e: int, f: int) -> int:
        return a + b + c + d + e + f

    assert foo(3, 4, 5, 7, 9, 11) == 39
