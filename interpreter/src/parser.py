import ply.yacc as yacc

from ast_nodes import *
from lexer import Lexer


class Parser(object):
    tokens = Lexer.tokens

    def p_program(self, p):
        '''program : statement_list'''
        p[0] = Node('program', children=p[1])

    def p_statement_list(self, p):
        '''statement_list : statement
                          | statement statement_list'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[2]

    def p_statement(self, p):
        '''statement : expression
                     | assignment'''
        p[0] = p[1]

    def p_expression(self, p):
        '''expression : if_else
                      | function_call
                      | function_definition
                      | block
                      | integer
                      | list
                      | ID
                      | function_name'''
        p[0] = p[1]

    def p_if_else(self, p):
        '''if_else : IF LPAREN expression RPAREN expression ELSE expression'''
        p[0] = IfElseNode(p[3], p[5], p[7])

    def p_assignment(self, p):
        '''assignment : ID ASSIGN expression'''
        p[0] = AssignmentNode(p[1], p[3])

    def p_function_call(self, p):
        '''function_call : function_name LPAREN argument_list RPAREN
                         | function_name LPAREN RPAREN'''
        if len(p) == 4:
            p[0] = FunctionCallNode(p[1], [])
        else:
            p[0] = FunctionCallNode(p[1], p[3])

    def p_function_definition(self, p):
        '''function_definition : PIPE parameter_list PIPE expression
                               | PIPE PIPE expression'''
        if len(p) == 4:
            params = []
            body = p[3]
        else:
            params = p[2]
            body = p[4]
        if not isinstance(body, BlockNode):
            body = BlockNode([], body)
        p[0] = FunctionDefinitionNode(params, body)

    def p_block(self, p):
        '''block : L_CURLY_BRACKET block_list R_CURLY_BRACKET'''
        return_val = p[2].pop()
        p[0] = BlockNode(p[2], return_val)

    def p_block_list(self, p):
        '''block_list : expression
                      | statement block_list'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[2]

    def p_integer(self, p):
        '''integer : NUMBER'''
        p[0] = IntegerNode(p[1])

    def p_list(self, p):
        '''list : L_SQUARE_BRACKET element_list R_SQUARE_BRACKET'''
        p[0] = ListNode(p[2])

    def p_argument_list(self, p):
        '''argument_list : expression
                         | expression COMMA argument_list'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    def p_parameter_list(self, p):
        '''parameter_list : ID
                          | ID COMMA parameter_list'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    def p_element_list(self, p):
        '''element_list : expression
                        | expression COMMA element_list'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    def p_function_name(self, p):
        '''function_name : ID
                         | AND
                         | CONCAT
                         | DIV
                         | EQ
                         | GREATER
                         | HEAD
                         | IS_EMPTY
                         | LESS
                         | MINUS
                         | MOD
                         | MULT
                         | NOT
                         | OR
                         | PLUS
                         | PRINT
                         | TAIL'''
        p[0] = p[1]

    def p_error(self, p):
        print("Syntax error!", end=' ')
        if not p:
            print("Unexpected end of file!")
            return
        print(f"At line {p.lineno}.", end=' ')
        print(f"Value: {p.value!r}")

    def build(self, **kwargs):
        self.parser = yacc.yacc(module=self, **kwargs)

    def parse(self, text):
        return self.parser.parse(text, lexer=Lexer().build())
