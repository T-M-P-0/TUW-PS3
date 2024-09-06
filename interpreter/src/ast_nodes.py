class Node(object):
    def __init__(self, node_type: str, children=None, value=None):
        self.node_type = node_type
        self.children = children or []
        self.value = value

    def __str__(self):
        return self.str_inner(0)

    def str_inner(self, depth=0):
        tabs = '\t' * depth

        val = f'{tabs}{self.node_type}\n'
        for child in self.children:
            if isinstance(child, Node):
                val += child.str_inner(depth + 1)
            else:
                val += '\t' * (depth + 1)
                val += str(child)
                val += '\n'
        return val


class AssignmentNode(Node):
    def __init__(self, name, new_val):
        super().__init__('assignment', children=[name, new_val])

    def str_inner(self, depth=0):
        tabs = '\t' * depth

        val = f'{tabs}{self.node_type} - {self.children[0]}\n'
        if isinstance(self.children[1], Node):
            val += self.children[1].str_inner(depth + 1)
        else:
            val += '\t' * (depth + 1)
            val += str(self.children[1])
            val += '\n'
        return val


class FunctionCallNode(Node):
    def __init__(self, name, args):
        super().__init__('function_call', children=[name, args])

    def str_inner(self, depth=0):
        tabs = '\t' * depth

        val = f'{tabs}{self.node_type} - {self.children[0]}\n'
        for child in self.children[1]:
            if isinstance(child, Node):
                val += child.str_inner(depth + 1)
            else:
                val += '\t' * (depth + 1)
                val += str(child)
                val += '\n'
        return val


class BlockNode(Node):
    def __init__(self, statements, return_val):
        super().__init__('block', children=[statements, return_val])

    def str_inner(self, depth=0):
        tabs = '\t' * depth

        val = f'{tabs}{self.node_type}\n'
        for child in self.children[0] + [self.children[1]]:
            if isinstance(child, Node):
                val += child.str_inner(depth + 1)
            else:
                val += '\t' * (depth + 1)
                val += str(child)
                val += '\n'
        return val


class IfElseNode(Node):
    def __init__(self, condition, expression1, expression2):
        super().__init__('if_else', children=[condition, expression1, expression2])


class FunctionDefinitionNode(Node):
    def __init__(self, params, body):
        super().__init__('function_definition', children=[params, body])

    def str_inner(self, depth=0):
        tabs = '\t' * depth

        val = f'{tabs}{self.node_type} - |{",".join(self.children[0])}|\n'
        if isinstance(self.children[1], Node):
            val += self.children[1].str_inner(depth + 1)
        else:
            val += '\t' * (depth + 1)
            val += str(self.children[2])
            val += '\n'
        return val


class IntegerNode(Node):
    def __init__(self, value):
        super().__init__('integer', value=value)

    def str_inner(self, depth=0):
        tabs = '\t' * depth
        return f'{tabs}{self.node_type} - {self.value}\n'


class ListNode(Node):
    def __init__(self, elements):
        super().__init__('list', children=elements)
