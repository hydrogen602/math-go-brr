from math_go_brrr import brrr


def test_road_to_loops_assignment():
    @brrr(dump_ir=True, dump_ast=True)
    def foo(a: int, b: int) -> int:
        c = a + b
        b = a
        return c + b

    assert foo(4, 5) == 13


if __name__ == "__main__":
    test_road_to_loops_assignment()
