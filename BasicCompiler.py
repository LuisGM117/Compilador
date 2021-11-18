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
    def __init__(self, error_name, details):
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        return result

class illegalCharError(Error):
    def __init__(self, details):
        super().__init__('Illegal Character', details )




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


#LEXER
class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = -1
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos += 1
        self.current_char = self.text[self.pos] if self.pos < len(self.text) else None

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
                char = self.current_char 
                self.advance()
                return [], illegalCharError(char)

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


# RUN
def run(text):
    lexer = Lexer(text)
    tokens, error = lexer.make_tokens()

    return tokens, error