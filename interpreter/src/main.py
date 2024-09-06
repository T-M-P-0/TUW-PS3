import argparse
from interpreter.interpreter import Interpreter
from lexer import Lexer
from parser import Parser


def run(code):
    lexer = Lexer()
    lexer.build()
    # lexer.test(code)

    parser = Parser()
    parser.build()
    ast = parser.parser.parse(code, lexer=lexer.lexer)
    # print(ast)

    # Create an instance of the interpreter
    interpreter = Interpreter()
    # Parse and execute the code
    interpreter.interpret(ast)


if __name__ == '__main__':
    # Set up argument parsing
    parser = argparse.ArgumentParser(description='Run a Python-like script')
    
    # Adding command-line arguments
    parser.add_argument('-c', '--code', type=str, help='Code to run directly')

    # Parse the arguments
    args = parser.parse_args()

    # Check if code is provided directly
    if args.code:
        run(args.code)
    # If a file is provided, read the file contents
    else:
        print("Please provide code with -c.")