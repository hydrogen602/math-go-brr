import inspect, ast, ast2json, json, re
from typing import Callable, Optional, overload, Protocol, Literal

from .math_go_brrr import *

ACCEPTED_TYPES: frozenset[type] = frozenset([int, bool])


# class FunctionToGoBrrr[**P, R: int | bool](Protocol):
#     def __call__(self, *args: P.args) -> R: ...


class FunctionGoneBrrr[**P, R: int | bool](Protocol):
    def __call__(
        self, *args: P.args  # pyright: ignore[reportGeneralTypeIssues]
    ) -> R: ...

    @property
    def original_func(self) -> Callable[P, R]: ...


@overload
def brrr[**P, R: int | bool](f: Callable[P, R]) -> FunctionGoneBrrr[P, R]: ...


@overload
def brrr[
    **P, R: int | bool
](
    *,
    dump_ir: bool = False,
    dump_ast: bool = False,
    dump_ast_json: bool = False,
    optimization: Literal["none", "less", "default", "aggressive"] = "default",
) -> Callable[[Callable[P, R]], FunctionGoneBrrr[P, R]]: ...


def brrr[  # pyright: ignore[reportInconsistentOverload]
    **P, R: int | bool
](
    f: Optional[Callable[P, R]] = None,
    /,
    *,
    dump_ir: bool = False,
    dump_ast: bool = False,
    dump_ast_json: bool = False,
    optimization: Literal["none", "less", "default", "aggressive"] = "default",
):
    """
    A decorator that compiles a function to LLVM IR and JIT compiles it to machine code.

    It can either be used directly as a decorator or with keyword arguments to configure the compilation.

    Args:
        f: The function to compile.
        dump_ir: Whether to dump the LLVM IR to stderr.
        dump_ast: Whether to dump the AST to stdout.
        dump_ast_json: Whether to dump the AST as JSON to stdout.
        optimization: The optimization level to use. Valid values are "none", "less", "default", and "aggressive".

    Returns:
        The compiled function. It has a property `original_func` that holds the original Python function.

    """
    opts = CompileOpts(dump_ir=dump_ir, optimization_level=optimization)

    def inner(f: Callable) -> FunctionGoneBrrr[P, R]:
        name = getattr(f, "__name__", repr(f))

        # print(f"make {name} go brrr")
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

        return take_source(ast_str, opts, f)

    if f is not None:
        return inner(f)
    else:
        # got config
        return inner
