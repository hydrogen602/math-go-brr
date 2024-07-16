from math_go_brrr import brrr
from .util import assert_compatible_for_all
import pytest


def test_int_div():
    @brrr(dump_ir=True)
    def foo(a: int, b: int) -> int:
        return a // b

    assert_compatible_for_all([(1, 2), (3, 4), (5, 6)], f=foo)
    assert_compatible_for_all(
        [(6, 3), (7, 3), (8, 3), (9, 3), (10, 3), (11, 3), (12, 3)], f=foo
    )

    # test negative numbers
    assert_compatible_for_all(
        [(-6, 3), (-7, 3), (-8, 3), (-9, 3), (-10, 3), (-11, 3), (-12, 3)], f=foo
    )
    assert_compatible_for_all(
        [(6, -3), (7, -3), (8, -3), (9, -3), (10, -3), (11, -3), (12, -3)], f=foo
    )

    with pytest.raises(ZeroDivisionError):
        foo(5, 0)


def test_int_mod():
    @brrr(dump_ir=True)
    def foo(a: int, b: int) -> int:
        return a % b

    assert_compatible_for_all([(1, 2), (3, 4), (5, 6)], f=foo)
    assert_compatible_for_all(
        [(6, 3), (7, 3), (8, 3), (9, 3), (10, 3), (11, 3), (12, 3)], f=foo
    )

    with pytest.raises(ZeroDivisionError):
        foo(5, 0)
