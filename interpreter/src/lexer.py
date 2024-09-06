import ply.lex as lex

# List of token names.   This is always required
reserved = {
    'if': 'IF',
    'else': 'ELSE',

    'plus': 'PLUS',
    'minus': 'MINUS',
    'mult': 'MULT',
    'div': 'DIV',
    'mod': 'MOD',

    'eq': 'EQ',
    'not': 'NOT',
    'and': 'AND',
    'or': 'OR',
    'less_than': 'LESS',
    'greater_than': 'GREATER',

    'print': 'PRINT',

    'head': 'HEAD',
    'tail': 'TAIL',
    'is_empty': 'IS_EMPTY',
    'concat': 'CONCAT',
}
tokens = [
             'COMMENT',
             'NUMBER',
             'PIPE',
             'LPAREN',
             'RPAREN',
             'L_SQUARE_BRACKET',
             'R_SQUARE_BRACKET',
             'L_CURLY_BRACKET',
             'R_CURLY_BRACKET',
             'COMMA',
             'ASSIGN',
             'ID'
         ] + list(reserved.values())


class Lexer(object):
    reserved = reserved
    tokens = tokens

    # Regular expression rules for simple tokens
    t_PIPE = r'\|'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_L_SQUARE_BRACKET = r'\['
    t_R_SQUARE_BRACKET = r'\]'
    t_L_CURLY_BRACKET = r'{'
    t_R_CURLY_BRACKET = r'}'
    t_COMMA = r','
    t_ASSIGN = r'='

    def __init__(self):
        self.lexer = None

    def t_ID(self, t):
        r'[a-z_]+'
        t.type = self.reserved.get(t.value, 'ID')  # Check for reserved words
        return t

    # A regular expression rule with some action code
    def t_NUMBER(self, t):
        r'\d+'
        t.value = int(t.value)
        return t

    # Ignore comments
    def t_COMMENT(self, t):
        r'\#.*'
        pass

    # A string containing ignored characters (spaces, tabs and newlines)
    t_ignore = ' \t\n'

    # Error handling rule
    def t_error(self, t):
        print(t)
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    # Build the lexer
    def build(self, **kwargs):
        self.lexer = lex.lex(module=self, **kwargs)

    # Test it output
    def test(self, data):
        self.lexer.input(data)
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            print(tok)
