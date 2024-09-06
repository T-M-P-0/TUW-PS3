from typing import Callable, List

from ast_nodes import FunctionCallNode, FunctionDefinitionNode, IntegerNode
from interpreter.types import Type, EvaluationResult


class FunctionExecutor:
    def __init__(self, name: str, parameters: list[Type | None],
                 function: Callable[[List[EvaluationResult]], EvaluationResult]):
        self.name = name
        self.parameters = parameters
        self.function = function

    def __call__(self, *args, **kwargs) -> EvaluationResult:
        assert len(self.parameters) >= len(args), \
            f"Invalid number of arguments. Expected less than {len(self.parameters)} arguments, got {len(args)}"
        assert len(kwargs) == 0

        params = []
        for i, param in enumerate(args):
            assert isinstance(param, EvaluationResult)
            if self.parameters[i] is not None:
                assert param.type == self.parameters[i], \
                    f"Function arguments for {self.name} do not match. Expected {self.parameters}, got {[x.type for x in args]}"
            params.append(param)

        if len(args) == len(self.parameters):
            result = self.function(params)
            return result
        else:
            # Create a new function like:
            #   mult(2) -> |__RESERVED__| mult(2, __RESERVED__)
            num_args = len(args)
            num_params_left = len(self.parameters) - num_args

            reserved_keywords = []
            for i in range(num_params_left):
                reserved_keywords.append(f"__RESERVED{i}__")

            new_args = []
            for arg in args:
                node = IntegerNode(arg.value)
                new_args.append(node)
            new_args = new_args + reserved_keywords

            new_body = FunctionCallNode(self.name, new_args)
            new_function = FunctionDefinitionNode(reserved_keywords, new_body)
            return EvaluationResult(Type.FUNCTION, new_function.children, stack=[])

    def __repr__(self):
        return f"FunctionExecutor({self.name}, {self.parameters})"
