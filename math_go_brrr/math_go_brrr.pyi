from typing import Literal

class CompileOpts:
    def __init__(
        self,
        dump_ir: bool,
        optimization_level: Literal[
            "none", "less", "default", "aggressive"
        ] = "default",
    ) -> None: ...
