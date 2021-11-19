# ADD CONSTANTS
DIGITS = '0123456789'

TOKEN_INT           = 'INT'
TOKEN_FLOAT         = 'FLOAT'
TOKEN_PLUS          = 'PLUS' # +
TOKEN_MINUS         = 'MINUS' # -
TOKEN_MUL           = 'MUL' # *
TOKEN_DIV           = 'DIV' # /
TOKEN_LEFTPAREN     = 'LEFTPAREN' # (
TOKEN_RIGHTPAREN    = 'RIGHTPAREN' # )
TOKEN_EOF           = 'EOF' # End Of File



#HANDLE ERROR 
#SYNTAX AND ILLEGAL CHARACTER
class Error:
    def __init__(self, start, end, error_name, details):
        self.start = start
        self.end = end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        return result

class illegalCharError(Error):
    def __init__(self, start, end, details):
        super().__init__(start, end,'Illegal Character', details )

class CompilerSyntaxError(Error):
    def __init__(self, start, end, details=''):
        super().__init__(start, end, 'Syntax Error', details)



# TOKENS
# TOKENS --> type - value (optional)
class Token:
    def __init__(self, type_, value=None, start=None, end=None):
        self.type = type_
        self.value = value

        if start:
            self.start = start.copyPosition()
            self.end = start.copyPosition()
            self.end.advance()

        
        if end:
            self.end = end.copyPosition()

    #Represents objects as a strings with __repr__
    def __repr__(self):
        if self.value: 
            return f'{self.type}:{self.value}'
        else:
            return f'{self.type}'






#POSITION
#A class to indicate the exactly place of the errors 
#Also will help the Parser Class

class Position:
    def __init__(self, index, line, column):
        self.index = index
        self.line = line
        self.column = column

    def advance(self, current_char=None):
        self.index += 1
        self.column += 1
        if current_char == '\n':
            self.line += 1
            self.column = 0
        return self

    def copyPosition(self):
        return Position(self.index, self.line, self.column)









#LEXER
class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = Position(-1, 0, -1)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

    def make_tokens(self):
        tokens = []

        while self.current_char != None:
            if self.current_char in ' \t': 
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char == '+':
                tokens.append(Token(TOKEN_PLUS, start = self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TOKEN_MINUS, start = self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TOKEN_MUL, start = self.pos))
                self.advance() 
            elif self.current_char == '/':
                tokens.append(Token(TOKEN_DIV, start = self.pos))
                self.advance() 
            elif self.current_char == '(':
                tokens.append(Token(TOKEN_LEFTPAREN, start = self.pos))
                self.advance() 
            elif self.current_char == ')':
                tokens.append(Token(TOKEN_RIGHTPAREN, start = self.pos))
                self.advance()
            else:
                start = self.pos.copyPosition()
                char = self.current_char 
                self.advance()
                return [], illegalCharError(start, self.pos, char)
        tokens.append(Token(TOKEN_EOF, start=self.pos))
        return tokens, None

    def make_number(self):
        num_str = ''
        dot_count = 0 
        #To check if is an integer or a float number


        start = self.pos.copyPosition()

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':  
                if dot_count == 1:  break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TOKEN_INT, int(num_str), start, self.pos)
        else:
            return Token(TOKEN_FLOAT, float(num_str), start, self.pos)





#NODES
class NumberNode:
    def __init__(self, tok): 
        self.tok = tok
        self.start = self.tok.start
        self.end = self.tok.end
    
    def __repr__(self):
        return f'{self.tok}'

class BinaryOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node  = left_node
        self.op_tok = op_tok
        self.right_node = right_node
        self.start = self.left_node.start
        self.end = self.right_node.end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'
    











# PARSER 
class Parser:
    def __init__(self, tokens): 
        self.tokens = tokens
        self.tok_index = -1
        self.advance()

    def advance(self, ):
        self.tok_index += 1
        if self.tok_index < len(self.tokens):
            self.current_tok = self.tokens[self.tok_index]
        return self.current_tok

    
    def parse(self):
        res = self.expression()
        if not res.error and self.current_tok.type != TOKEN_EOF:
                return res.failure(CompilerSyntaxError(self.current_tok.start, self.current_tok.end, 
                "Expected a character: '+' || '-' || '*' || '/' between numbers "))
        return res

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TOKEN_INT, TOKEN_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))

        elif tok.type == TOKEN_LEFTPAREN:
            res.register(self.advance())
            expr = res.register(self.expression())
            if res.error: return res
            if self.current_tok.type == TOKEN_RIGHTPAREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(CompilerSyntaxError(self.current_tok.start, self.current_tok.end,
                 "Expected ')' "))

        return res.failure(CompilerSyntaxError(self.current_tok.start, self.current_tok.end,
         "Check if you have entered all parameters"))

        

    def term(self):
       return self.bin_op(self.factor, (TOKEN_MUL, TOKEN_DIV))

    def expression(self):
        return self.bin_op(self.term, (TOKEN_PLUS, TOKEN_MINUS))

    def bin_op(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res

        while self.current_tok.type in ops:
            op_tok = self.current_tok
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res
            left = BinaryOpNode(left, op_tok, right)
        
        return res.success(left) 


#PARSE RESULT 

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error: self.error = res.error
            return res.node
        return res

    def success(self, node):
        self.node = node
        return self
    

    def failure(self, error):
        self.error = error
        return self


#NUMBER CLASS 
#TO GET ACTUAL VALUES
class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()
    
    def set_pos(self, start = None, end = None ):
        self.start = start
        self.end = end
        return self

    #isinstance return true if two parameters are the same type or same objects    

    def added_to(self, other): 
        if isinstance(other, Number): 
            return Number(self.value + other.value)
    
    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value)

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value)

    def divided_by(self, other):
        if isinstance(other, Number):
            return Number(self.value / other.value)

    def __repr__(self):
        return str(self.value)

#INTERPRETER
#MAKE OPERATIONS AND PRINT THE RESULT

class Interpreter:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)

    def no_visit_method(self, node):
        raise Exception(f'No visit_{type(node).__name__} method defined')


    def visit_NumberNode(self, node):
        return Number(node.tok.value).set_pos(node.start, node.end)

    def visit_BinaryOpNode(self, node):
        left = self.visit(node.left_node)
        right = self.visit(node.right_node)

        if node.op_tok.type == TOKEN_PLUS:
            result = left.added_to(right)
        elif node.op_tok.type == TOKEN_MINUS:
            result = left.subbed_by(right)
        elif node.op_tok.type == TOKEN_MUL:
            result = left.multed_by(right)
        elif node.op_tok.type == TOKEN_DIV:
            result = left.divided_by(right)

        return result.set_pos(node.start, node.end)
# RUN
def run(text):
    lexer = Lexer(text)
    tokens, error = lexer.make_tokens()
    if error: return None, error

    #GENERATE AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    #RUN PROGRAM
    interpreter = Interpreter()
    result = interpreter.visit(ast.node)

    return result, None