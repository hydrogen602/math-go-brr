from math_go_brrr import brrr
import pytest


def test_road_to_loops_assignment():
    @brrr(dump_ir=True, dump_ast=True)
    def foo(a: int, b: int) -> int:
        c = a + b
        b = a
        return c + b

    assert foo(4, 5) == 13

    @brrr(dump_ir=True, dump_ast=True)
    def foo() -> int:
        c = 10
        d = 5
        e = 3
        return c + d

    assert foo() == 15


def test_road_to_loops_booleans():
    @brrr
    def foo(a: bool, b: bool) -> bool:
        return a

    assert foo(True, False) == True
    assert foo(False, False) == False
    assert foo(True, True) == True
    assert foo(False, True) == False

    @brrr
    def foo(a: bool, b: bool) -> bool:
        return b

    assert foo(True, False) == False
    assert foo(False, False) == False
    assert foo(True, True) == True
    assert foo(False, True) == True

    @brrr
    def foo(i: int, a: bool) -> bool:
        b = a
        return b

    assert foo(42, True) == True
    assert foo(10000000, False) == False

    @brrr
    def foo(a: bool) -> bool:
        b = True
        return b

    assert foo(False) == True
    assert foo(True) == True


def test_road_to_loops_booleans_errors():
    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: bool) -> bool:
            a = b
            return b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int) -> bool:
            return a

    with pytest.raises(TypeError):

        @brrr
        def foo(a: bool) -> int:
            return a

    with pytest.raises(TypeError):

        @brrr
        def foo(a: bool) -> bool:
            return 1

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int) -> bool:
            return a
