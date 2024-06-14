"""
 |  end_lineno
 |      exception end lineno
 |
 |  end_offset
 |      exception end offset
 |
 |  filename
 |      exception filename
 |
 |  lineno
 |      exception lineno
 |
 |  msg
 |      exception msg
 |
 |  offset
 |      exception offset
 |
 |  print_file_and_line
 |      exception print_file_and_line
 |
 |  text
 |      exception text
"""


def foo():
    s = SyntaxError("A")
    s.lineno = 28
    s.end_lineno = 28
    s.offset = 5
    s.end_offset = 8

    s.filename = "test.py"
    with open("test.py") as f:
        s.text = f.read().splitlines()[28 - 1]
    raise s


foo()
