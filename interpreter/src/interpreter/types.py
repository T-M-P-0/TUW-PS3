from enum import Enum
from typing import Optional


class Type(Enum):
    # has value of True/False
    BOOLEAN = 0
    # has value of 0,1,...
    INTEGER = 1
    # has value of (params[], code)
    FUNCTION = 2
    # has value of values[]
    LIST = 3
    # use for functions with side effects like print (as they have no return values)
    UNIT = 4


class IEnvironment:
    def __init__(self, variables=None):
        if variables is None:
            variables = {}
        self.variables = variables

    def get_variable(self, name):
        pass

    def set_variable(self, name, result):
        pass


class EvaluationResult:
    def __init__(self, type: Type, value: any, stack: Optional[list[IEnvironment]] = None):
        self.type = type
        self.value = value
        self.stack = stack

    def __repr__(self):
        return f"EvaluationResult(type={self.type}, value={self.value}, stack={self.stack})"


class Environment(IEnvironment):
    def __init__(self, variables=None):
        super().__init__(variables)

    def get_variable(self, name: str) -> Optional[EvaluationResult]:
        if name in self.variables:
            return self.variables[name]
        return None

    def set_variable(self, name: str, result: EvaluationResult):
        self.variables[name] = result
