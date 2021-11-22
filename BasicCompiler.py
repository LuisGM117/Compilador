import string

# ADD CONSTANTS
DIGITS = '0123456789'
LETTERS = string.ascii_letters # a-z letters
LETTERS_DIGITS = LETTERS + DIGITS

TOKEN_INT           = 'INT'
TOKEN_FLOAT         = 'FLOAT'
TOKEN_PLUS          = 'PLUS' # +
TOKEN_MINUS         = 'MINUS' # -
TOKEN_MUL           = 'MUL' # *
TOKEN_DIV           = 'DIV' # /
TOKEN_POW           = 'POW' # ^
TOKEN_LEFTPAREN     = 'LEFTPAREN' # (
TOKEN_RIGHTPAREN    = 'RIGHTPAREN' # )
TOKEN_EOF           = 'EOF' # End Of File
TOKEN_IDENTIFIER    = 'IDENTIFIER'
TOKEN_KEYWORD       = 'KEYWORD'
TOKEN_EQUALS        = 'EQUALS'


KEYWORDS = ['VAR'] 

#HANDLE ERROR 
#SYNTAX AND ILLEGAL CHARACTER
class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        return result

class illegalCharError(Error):
    def __init__(self, start, end, details):
        super().__init__(start, end,'Illegal Character', details )

class CompilerSyntaxError(Error):
    def __init__(self, start, end, details=''):
        super().__init__(start, end, 'Syntax Error', details)


class RTError(Error):
	def __init__(self, pos_start, pos_end, details, context):
		super().__init__(pos_start, pos_end, 'Runtime Error', details)
		self.context = context

	def as_string(self):
		result  = self.generate_traceback()
		result += f'{self.error_name}: {self.details}'
		return result

	def generate_traceback(self):
         result = ''
         pos = self.pos_start
         ctx = self.context
         while ctx:
            result = f' File {pos.filename}, line{str(pos.line + 1)}, in {ctx.disply_name}\n' + result
            pos = ctx.paren_entry_pos
            ctx = ctx.parent



# TOKENS
# TOKENS --> type - value (optional)
class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copyPosition()
            self.pos_end = pos_start.copyPosition()
            self.pos_end.advance()

        
        if pos_end:
            self.end = pos_end.copyPosition()

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

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
    def __init__(self, index, line, column, filename, filetext):
        self.index = index
        self.line = line
        self.column = column
        self.filename = filename
        self.filetext = filetext

    def advance(self, current_char=None):
        self.index += 1
        self.column += 1
        if current_char == '\n':
            self.line += 1
            self.column = 0
        return self

    def copyPosition(self):
        return Position(self.index, self.line, self.column, self.filename, self.filetext)









#LEXER
class Lexer:
    def __init__(self, filename, text):
        self.filename = filename
        self.text = text
        self.pos = Position(-1, 0, -1, filename, text)
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
            #CHECK IF THE CURRENT CHARACTER IS A LETTER
            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
            elif self.current_char == '+':
                tokens.append(Token(TOKEN_PLUS, pos_start = self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TOKEN_MINUS, pos_start = self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TOKEN_MUL, pos_start = self.pos))
                self.advance() 
            elif self.current_char == '/':
                tokens.append(Token(TOKEN_DIV, pos_start = self.pos))
                self.advance()
            elif self.current_char == '^':
                tokens.append(Token(TOKEN_POW, pos_start = self.pos))
                self.advance()  
            elif self.current_char == '(':
                tokens.append(Token(TOKEN_LEFTPAREN, pos_start = self.pos))
                self.advance() 
            elif self.current_char == ')':
                tokens.append(Token(TOKEN_RIGHTPAREN, pos_start = self.pos))
                self.advance()
            elif self.current_char == '=':
                tokens.append(Token(TOKEN_EQUALS, pos_start = self.pos))
                self.advance()
            else:
                pos_start = self.pos.copyPosition()
                char = self.current_char 
                self.advance()
                return [], illegalCharError(pos_start, self.pos, char)
        tokens.append(Token(TOKEN_EOF, pos_start=self.pos))
        return tokens, None

    def make_number(self):
        num_str = ''
        dot_count = 0 
        #To check if is an integer or a float number

        pos_start = self.pos.copyPosition()

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':  
                if dot_count == 1:  break
                dot_count += 1
            num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TOKEN_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TOKEN_FLOAT, float(num_str), pos_start, self.pos)

    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copyPosition()

        while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.advance()

        tok_type = TOKEN_KEYWORD if id_str in KEYWORDS else TOKEN_IDENTIFIER
        return Token(tok_type, id_str, pos_start, self.pos)



#NODES
class NumberNode:
    def __init__(self, tok): 
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
    
    def __repr__(self):
        return f'{self.tok}'

class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.var_name_tok.pos_end

class VarAssignNode:
	def __init__(self, var_name_tok, value_node):
		self.var_name_tok = var_name_tok
		self.value_node = value_node

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.value_node.pos_end

class BinaryOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node  = left_node
        self.op_tok = op_tok
        self.right_node = right_node
        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'
    


class UnaryOpNode():
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = node.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'








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
                return res.failure(CompilerSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, 
                "Expected a character: '+' || '-' || '*' || '/' between numbers "))
        return res


    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TOKEN_INT, TOKEN_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))

        elif tok.type == TOKEN_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))


        elif tok.type == TOKEN_LEFTPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expression())
            if res.error: return res
            if self.current_tok.type == TOKEN_RIGHTPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(CompilerSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                 "Expected ')' "))
        
        return res.failure(CompilerSyntaxError(tok.pos_start, tok.pos_end, 
                "Expected int, float, identifier, \n or a character: '+' || '-' || '*' || '/' between numbers "))
        
    def power(self):
        return self.bin_op(self.atom, (TOKEN_POW, ), self.factor)

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TOKEN_PLUS, TOKEN_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))
        
        return self.power()

        

    def term(self):
       return self.bin_op(self.factor, (TOKEN_MUL, TOKEN_DIV))

    def expression(self):
        res = ParseResult()
        if self.current_tok.matches(TOKEN_KEYWORD, 'VAR'):
            res.register_advancement()
            self.advance()
            
            if self.current_tok.type != TOKEN_IDENTIFIER:
                return res.failure(CompilerSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expcted identifier"
                ))
            
            var_name = self.current_tok
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TOKEN_EQUALS:
                return res.failure(CompilerSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected equals '=' symbol "  ))
            res.register_advancement()
            self.advance()
            expr = res.register(self.expression())
            if res.error: return res
            return res.success(VarAssignNode(var_name, expr))


        node = res.register(self.bin_op(self.term, (TOKEN_PLUS, TOKEN_MINUS)))

        if res.error: 
            return res.failure(CompilerSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected Identifier"))

        return res.success(node)

    def bin_op(self, funca, ops, funcb=None):
        if funcb == None:
            funcb = funca
        res = ParseResult()
        left = res.register(funca())
        if res.error: return res

        while self.current_tok.type in ops:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(funcb())
            if res.error: return res
            left = BinaryOpNode(left, op_tok, right)
        
        return res.success(left) 






#PARSE RESULT 
class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0
    
    def register_advancement(self):
        self.advance_count += 1

    def register(self, res):
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node
         

    def success(self, node):
        self.node = node
        return self
    

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self





class RuntimeResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error: 
            self.error = res.error
        return res.value

    def success(self, value):
        self.value = value
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
        self.set_context()
    
    def set_pos(self, pos_start = None, pos_end = None ):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self


    #isinstance return true if two parameters are the same type or same objects    

    def added_to(self, other): 
        if isinstance(other, Number): 
            return Number(self.value + other.value).set_context(self.context), None
    
    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None

    def divided_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                print("You can't divided by zero")
            return Number(self.value / other.value).set_context(self.context), None
    
    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None

    def copyPosition(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)



#Symboll table
#For variables!

class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value):
        self.symbols[name] = value
    
    def remove(self, name):
        del self.symbols[name]



#CONTEXT CLASS
#ALSO WILL HELP VARIABLES

class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent 
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None





#INTERPRETER
#MAKE OPERATIONS AND PRINT THE RESULT

class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')


    def visit_NumberNode(self, node, context):
        return RuntimeResult().success(Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))  

    def visit_VarAccessNode(self, node, context):
        res = RuntimeResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(RTError(
                node.pos_start, node.pos_end, f" '{var_name}' is not defined", context
            ))
        value = value.copyPosition().set_pos(node.pos_start, node.pos_end)
        return res.success(value)

    def visit_VarAssignNode(self, node, context):
        res = RuntimeResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error: return res

        context.symbol_table.set(var_name, value)
        return res.success(value)

    def visit_BinaryOpNode(self, node, context):
        res = RuntimeResult()
        left = res.register(self.visit(node.left_node, context))
        if res.error:
            return res
        right = res.register(self.visit(node.right_node, context))
        if res.error:
            return res

        if node.op_tok.type == TOKEN_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TOKEN_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TOKEN_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TOKEN_DIV:
            result, error = left.divided_by(right)
        elif node.op_tok.type == TOKEN_POW:
            result, error = left.powed_by(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node, context):
        res = RuntimeResult()
        number = res.register(self.visit(node.node, context))
        if res.error: return res

        error = None

        if node.op_tok.type == TOKEN_MINUS:
            number, error = number.multed_by(Number(-1))
        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start. node.pos_end))







global_symbolTable = SymbolTable()
global_symbolTable.set("null", Number(0))

# RUN
def run(filename, text):
    lexer = Lexer(filename, text)
    tokens, error = lexer.make_tokens()
    if error: return None, error

    #GENERATE AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    #RUN PROGRAM
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbolTable
    result = interpreter.visit(ast.node, context)

    return result.value, result.error