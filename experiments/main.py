import ast
import ast2json
import json
from sys import stdout

if __name__ == "__main__":
    # read example.py
    with open("example.py", "r") as f:
        code = f.read()
    # parse ast

    tree = ast.parse(code)
    js = ast2json.ast2json(tree)

    json.dump(js, stdout, indent=2)
