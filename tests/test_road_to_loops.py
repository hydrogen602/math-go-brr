from math_go_brrr import brrr
import pytest
from .util import assert_compatible_for_all, all_variations_3


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


def test_road_to_loops_too_many_segfaults():
    """
    I've been getting SIGSEGV or SIGABORTS
    from bad code generation related to terminal
    statements in llvm IR.
    """

    @brrr
    def foo(a: bool) -> int:
        b = True

    assert foo(True) == 0

    @brrr
    def foo(a: bool) -> int:
        return 42

    assert foo(True) == 42

    # it still has to work no matter where there are return statements or not

    @brrr
    def foo(a: bool) -> int:
        if a:
            return 42
        else:
            return -42
        return 0

    assert foo(True) == 42
    assert foo(False) == -42

    @brrr
    def foo(a: bool) -> int:
        b = 3
        if a:
            b = 4
        else:
            return -42
        return 0

    assert foo(True) == 0
    assert foo(False) == -42

    @brrr
    def foo(a: bool) -> int:
        b = 3
        if a:
            return 42
        else:
            b = 4
        return 0

    assert foo(True) == 42
    assert foo(False) == 0

    @brrr
    def foo(a: bool) -> int:
        if a:
            return 42
        else:
            return -42

    assert foo(True) == 42
    assert foo(False) == -42

    @brrr
    def foo(a: bool) -> int:
        b = 4
        if a:
            b = 1
        else:
            return -42

    assert foo(True) == 0
    assert foo(False) == -42


def test_road_to_loops_conditional_flow():

    @brrr(dump_ir=True)
    def foo(a: bool) -> int:
        if a:
            return 42
        else:
            return -42

    assert foo(True) == 42
    assert foo(False) == -42

    @brrr
    def foo(a: bool) -> int:
        if a:
            return 42
        return -42

    assert foo(True) == 42
    assert foo(False) == -42

    @brrr
    def foo(a: bool) -> int:
        b = 0
        if a:
            b = 42
        return b + 1

    assert foo(True) == 43
    assert foo(False) == 1


def test_road_to_loops_conditional_flow_restrictions():
    with pytest.raises(SyntaxError):

        # declaring new variables in the if block
        # makes things very messy and dangling pointers likely
        @brrr(dump_ast_json=True)
        def foo(a: bool) -> int:
            if a:
                b = 4
            else:
                pass

    with pytest.raises(TypeError):

        # b is implicitly declared as i64
        @brrr
        def foo(a: bool) -> int:
            b = 4
            if a:
                b = a
            else:
                pass


def test_road_to_loops_bool_ops():
    @brrr
    def foo(a: bool, b: bool) -> bool:
        return a and b

    assert foo(True, True) == True
    assert foo(True, False) == False
    assert foo(False, True) == False
    assert foo(False, False) == False

    @brrr
    def foo(a: bool, b: bool) -> bool:
        return a or b

    assert foo(True, True) == True
    assert foo(True, False) == True
    assert foo(False, True) == True
    assert foo(False, False) == False

    @brrr
    def foo(a: bool) -> bool:
        return not a

    assert foo(True) == False
    assert foo(False) == True

    @brrr
    def foo(a: bool, b: bool) -> bool:
        return a and not b

    assert foo(True, True) == False
    assert foo(True, False) == True
    assert foo(False, True) == False
    assert foo(False, False) == False

    @brrr
    def foo(a: bool, b: bool) -> bool:
        return a or not b

    assert foo(True, True) == True
    assert foo(True, False) == True
    assert foo(False, True) == False
    assert foo(False, False) == True

    @brrr
    def foo(a: bool, b: bool) -> bool:
        return not a or b

    assert foo(True, True) == True
    assert foo(True, False) == False
    assert foo(False, True) == True
    assert foo(False, False) == True

    @brrr
    def foo(a: bool, b: bool, c: bool, d: bool) -> bool:
        return a and b and c and d

    assert foo(True, True, True, True) == True
    assert foo(True, True, True, False) == False

    @brrr
    def foo(a: bool, b: bool, c: bool, d: bool) -> bool:
        return a and b or c and d

    assert foo(True, True, True, True) == True
    assert foo(True, True, True, False) == True


def test_road_to_loops_comparisons():
    @brrr(dump_ast_json=True)
    def foo(a: int, b: int) -> bool:
        return a == b

    assert foo(42, 42) == True
    assert foo(42, 43) == False

    @brrr
    def foo(a: int, b: int) -> bool:
        return a != b

    assert foo(42, 42) == False
    assert foo(42, 43) == True

    @brrr
    def foo(a: int, b: int) -> bool:
        return a < b

    assert foo(42, 42) == False
    assert foo(42, 43) == True
    assert foo(43, 42) == False

    @brrr
    def foo(a: int, b: int) -> bool:
        return a > b

    assert foo(42, 42) == False
    assert foo(42, 43) == False
    assert foo(43, 42) == True

    @brrr
    def foo(a: int, b: int) -> bool:
        return a <= b

    assert foo(42, 42) == True
    assert foo(42, 43) == True
    assert foo(43, 42) == False

    @brrr
    def foo(a: int, b: int) -> bool:
        return a >= b

    assert foo(42, 42) == True
    assert foo(42, 43) == False
    assert foo(43, 42) == True


def test_road_to_loops_comparisons_chain():

    @brrr
    def foo(a: int, b: int, c: int) -> bool:
        return a < b < c

    assert_compatible_for_all(all_variations_3(1, 2, 3), f=foo)

    @brrr
    def foo(a: int, b: int, c: int) -> bool:
        return a < b <= c

    assert_compatible_for_all(all_variations_3(1, 2, 3), f=foo)

    @brrr
    def foo(a: int, b: int, c: int) -> bool:
        return a >= b < c

    assert_compatible_for_all(all_variations_3(1, 2, 3), f=foo)

    @brrr
    def foo(a: int, b: int, c: int) -> bool:
        return a >= b <= c - 1 or a == b

    assert_compatible_for_all(all_variations_3(1, 2, 3), f=foo)


def test_road_to_loops_bad_comparisons():
    with pytest.raises(TypeError):

        @brrr
        def foo(a: bool, b: bool) -> bool:
            return a < b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: bool, b: int) -> bool:
            return a > b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: bool) -> bool:
            return a <= b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: int) -> int:
            return a >= b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: bool) -> bool:
            return a and b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: bool, b: int) -> bool:
            return a or b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: int, b: int) -> bool:
            return a or b

    with pytest.raises(TypeError):

        @brrr
        def foo(a: bool, b: bool) -> int:
            return a and b


def test_loops():
    @brrr(dump_ast_json=True)
    def foo(a: int) -> int:
        b = 0
        i = 0
        while i < a:
            b = b + i
            i = i + 1

        return b

    assert_compatible_for_all([(e,) for e in range(5)], f=foo)
