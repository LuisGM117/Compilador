#1. Tokens

# TOKENS --> type - value (optional)

#ADD CONSTANTS
TOKEN_INT = 'INT'
TOKEN_FLOAT = 'FLOAT'
TOKEN_PLUS = 'PLUS' # +
TOKEN_MINUS = 'MINUS' # -
TOKEN_MUL = 'MUL' # x
TOKEN_DIV = 'DIV' # /
TOKEN_LEFTPAREN = 'LEFTPAREN' # (
TOKEN_RIGHTPAREN = 'RIGHTPAREN' # )


class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    #Represents objects as a strings with __repr__
    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
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
        if self.pos < len(self.text):
            self.current_char = self.text[pos]
        else:
            None
            