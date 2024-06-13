import inspect, ast, ast2json, json, re
from typing import Callable, Optional
from .math_go_brrr import take_source, CompileOpts


ACCEPTED_TYPES: frozenset[type] = frozenset([int, bool])


def brrr(
    f: Optional[Callable] = None,
    /,
    *,
    dump_ir: bool = False,
    dump_ast: bool = False,
    dump_ast_json: bool = False,
):
    """
    Setup so it can both be used like:
    .. code-block:: python
    @brrr
    def foo(): ...

    @brrr(dump_ir=True)
    def foo(): ...

    """
    opts = CompileOpts(dump_ir=dump_ir)

    def inner(f: Callable):
        name = getattr(f, "__name__", repr(f))

        print(f"make {name} go brrr")
        if not callable(f):
            raise TypeError(
                f"Expected a callable, but got {type(f)} while making {name} go brrr"
            )

        anno = inspect.get_annotations(f)

        sig = inspect.signature(f)
        params = sig.parameters

        def nice_accepted_writer():
            match list(ACCEPTED_TYPES):
                case []:
                    return ""
                case [t]:
                    return t.__name__
                case [*rest, t]:
                    return ", ".join(t.__name__ for t in rest) + " or " + t.__name__

        for p in params:
            annotation = anno.get(p)
            if annotation not in ACCEPTED_TYPES:
                raise TypeError(
                    f"Expected parameter {p} to be {nice_accepted_writer()}, but got {annotation} while making {name} go brrr"
                )

        return_type = sig.return_annotation
        if return_type not in ACCEPTED_TYPES:
            raise TypeError(
                f"Expected return type to be {nice_accepted_writer()}, but got {return_type} while making {name} go brrr"
            )

        code = inspect.getsource(f)

        m = re.match(r"^([\t ]+)", code)
        if m:
            space = m.group(1)
            code = re.sub(f"^{space}", "", code, flags=re.MULTILINE)

        tree = ast.parse(code)
        if dump_ast:
            print(ast.dump(tree, indent=2))

        js = ast2json.ast2json(tree)
        ast_str = json.dumps(js, indent=2)

        if dump_ast_json:
            print(ast_str)

        return take_source(ast_str, opts)

    if f is not None:
        return inner(f)
    else:
        # got config
        return inner
