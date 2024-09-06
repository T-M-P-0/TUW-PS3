from interpreter.function_executor import FunctionExecutor
from interpreter.types import EvaluationResult, Type


def _mult(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value * params[1].value
    return EvaluationResult(Type.INTEGER, result)


def _eq(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].type == params[1].type and params[0].value == params[1].value
    return EvaluationResult(Type.BOOLEAN, result)


def _not(params: list[EvaluationResult]) -> EvaluationResult:
    result = not params[0].value
    return EvaluationResult(Type.BOOLEAN, result)


def _and(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value and params[1].value
    return EvaluationResult(Type.BOOLEAN, result)


def _or(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value and params[1].value
    return EvaluationResult(Type.BOOLEAN, result)


def _less_than(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value < params[1].value
    return EvaluationResult(Type.BOOLEAN, result)


def _greater_than(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value > params[1].value
    return EvaluationResult(Type.BOOLEAN, result)


def _plus(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value + params[1].value
    return EvaluationResult(Type.INTEGER, result)


def _minus(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value - params[1].value
    return EvaluationResult(Type.INTEGER, result)


def _div(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value // params[1].value
    return EvaluationResult(Type.INTEGER, result)


def _mod(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value % params[1].value
    return EvaluationResult(Type.INTEGER, result)


def _head(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value[0]
    return result


def _tail(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value[1:]
    return EvaluationResult(Type.LIST, result)


def _is_empty(params: list[EvaluationResult]) -> EvaluationResult:
    result = len(params[0].value) == 0
    return EvaluationResult(Type.BOOLEAN, result)


def _concat(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[0].value + params[1].value
    return EvaluationResult(Type.LIST, result)


def _if(params: list[EvaluationResult]) -> EvaluationResult:
    result = params[1] if params[0].value else params[2]
    return EvaluationResult(result.type, result)


def _as_str(params: EvaluationResult) -> str:
    type = params.type
    if type == Type.INTEGER:
        return params.value
    elif type == Type.BOOLEAN:
        return params.value
    elif type == Type.LIST:
        return str([_as_str(x) for x in params.value])
    elif type == Type.FUNCTION:
        return f"Function({', '.join(params.value[0])})"
    elif type == Type.UNIT:
        return "()"
    else:
        raise Exception("Unexpected type")


def _print(params: list[EvaluationResult]) -> EvaluationResult:
    print(_as_str(params[0]))
    return EvaluationResult(Type.UNIT, None)


functions = {
    "eq": FunctionExecutor("eq", [None, None], _eq),
    "not": FunctionExecutor("not", [Type.BOOLEAN], _not),
    "and": FunctionExecutor("and", [Type.BOOLEAN, Type.BOOLEAN], _and),
    "or": FunctionExecutor("or", [Type.BOOLEAN, Type.BOOLEAN], _or),
    "less_than": FunctionExecutor("less_than", [Type.INTEGER, Type.INTEGER], _less_than),
    "greater_than": FunctionExecutor("greater_than", [Type.INTEGER, Type.INTEGER], _greater_than),

    "plus": FunctionExecutor("plus", [Type.INTEGER, Type.INTEGER], _plus),
    "minus": FunctionExecutor("minus", [Type.INTEGER, Type.INTEGER], _minus),
    "mult": FunctionExecutor("mult", [Type.INTEGER, Type.INTEGER], _mult),
    "div": FunctionExecutor("div", [Type.INTEGER, Type.INTEGER], _div),
    "mod": FunctionExecutor("mod", [Type.INTEGER, Type.INTEGER], _mod),

    "head": FunctionExecutor("head", [Type.LIST], _head),
    "tail": FunctionExecutor("tail", [Type.LIST], _tail),
    "is_empty": FunctionExecutor("is_empty", [Type.LIST], _is_empty),
    "concat": FunctionExecutor("concat", [Type.LIST, Type.LIST], _concat),

    "print": FunctionExecutor("print", [None], _print),
}


def get_function(name) -> FunctionExecutor:
    return functions.get(name)
