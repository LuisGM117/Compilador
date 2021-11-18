# TOKENS
# TOKENS --> type - value (optional)

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




class Token:
    def __init__(self, type_, value=None):
        self.type = type_
        self.value = value

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
                tokens.append(Token(TOKEN_PLUS))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TOKEN_MINUS))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TOKEN_MUL))
                self.advance() 
            elif self.current_char == '/':
                tokens.append(Token(TOKEN_DIV))
                self.advance() 
            elif self.current_char == '(':
                tokens.append(Token(TOKEN_LEFTPAREN))
                self.advance() 
            elif self.current_char == ')':
                tokens.append(Token(TOKEN_RIGHTPAREN))
                self.advance()
            else:
                start = self.pos.copyPosition()
                char = self.current_char 
                self.advance()
                return [], illegalCharError(start, self.pos, char)

        return tokens, None

    def make_number(self):
        num_str = ''
        dot_count = 0 
        #To check if is an integer or a float number

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':  
                if dot_count == 1:  break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TOKEN_INT, int(num_str))
        else:
            return Token(TOKEN_FLOAT, float(num_str))





#NODES

class NumberNode:
    def __init__(self, tok): 
        self.tok = tok
    
    def __repr__(self):
        return f'{self.tok}'

class BinaryOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node  = left_node
        self.op_tok = op_tok
        self.right_node = right_node

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
        return res

    def factor(self):
        tok = self.current_tok

        if tok.type in (TOKEN_INT, TOKEN_FLOAT):
            self.advance()
            return NumberNode(tok)

    def term(self):
       return self.bin_op(self.factor, (TOKEN_MUL, TOKEN_DIV))

    def expression(self):
        return self.bin_op(self.term, (TOKEN_PLUS, TOKEN_MINUS))

    def bin_op(self, func, ops):
        left = func()

        while self.current_tok.type in ops:
            op_tok = self.current_tok
            self.advance()
            right = func()
            left = BinaryOpNode(left, op_tok, right)
        
        return left


# RUN
def run(text):
    lexer = Lexer(text)
    tokens, error = lexer.make_tokens()
    if error: return None, error

    #GENERATE AST
    parser = Parser(tokens)
    ast = parser.parse()

    return ast, error