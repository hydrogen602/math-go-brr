from math_go_brrr_py import brrr
import pytest


def test_type_errors_args_are_int():
    with pytest.raises(TypeError):

        @brrr
        def foo(a, b):
            return a

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b):
            return a

    with pytest.raises(TypeError):

        @brrr
        def foo(a, b: int):
            return a

    with pytest.raises(TypeError):

        @brrr
        def foo(a: str, b: int):
            return a

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: str):
            return a


def test_type_errors_return_is_int():
    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: int):
            return a

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: int) -> str:
            return a

    @brrr
    def foo(a: int, b: int) -> int:
        return a
