from types import TracebackType


def generate(
    filename: str, function_name: str, linenumber: int, offset: int, width: int
) -> TracebackType:
    """
    Note: the generated pointer is min 3 wide.
    """
    if width > 1:
        offset += 1

    err_generator = "1/" + "0" * max(width - 2, 1)  # "eval('%')"  #

    code = compile(
        "{}def {}():\n {}{}".format(
            "\n" * (linenumber - 2),
            function_name,
            " " * (offset - 2),
            err_generator,
        ),
        filename,
        "exec",
    )
    namespace = {}
    exec(code, namespace)
    function_name_fake_func = namespace[function_name]
    try:
        function_name_fake_func()
    except (SyntaxError, ZeroDivisionError) as fake_e:
        # here we silence our fake exception but keep the traceback
        tb = fake_e.__traceback__
        if tb.tb_next:
            tb = tb.tb_next
        return tb

    raise Exception("Control flow should not reach here")


# try:
#     name
# except NameError as e:
#     e.__traceback__ = generate(
#         "/Users/Jonathan/Documents/GitHub/Rust/math-go-brr/err.py", "bar", 11, 6, 3
#     )
#     raise
