from math_go_brrr import brrr


def test_reuse():
    @brrr
    def foo(a: int, b: int) -> int:
        return a + b + b

    assert foo(1, 2) == 5

    f = foo

    @brrr
    def foo(a: int, b: int) -> int:
        return a + b

    assert foo(1, 2) == 3

    assert f(1, 2) == 5
