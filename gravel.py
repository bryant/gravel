from llvm.core import Type, Module, Builder, Function, Constant
import re

_camelre = re.compile(r"([A-Z]+)(?=[0-9a-z])")

def camel2underscore(camel):
    def insert(match):
        m = match.group(0).lower()
        return "_" + (m if len(m) == 1 else m[:-1] + "_" + m[-1])
    return _camelre.sub(insert, camel).lstrip("_")

class UnknownTypeError(Exception):
    pass

class UnknownNodeType(Exception):
    pass

class Statement(object):
    pass

class Expression(Statement):
    pass

class IntLiteral(Expression):
    def __init__(self, value):
        self.value = int(value)

class Variable(Expression):
    def __init__(self, varname):
        self.name = varname

class FuncCall(Expression):
    def __init__(self, funcname, args):
        self.funcname, self.args = funcname, args

class BinaryExpr(Expression):
    operators = tuple("or and < <= > >= != == | ^ & << >> + - / * %".split())

    def __init__(self, op, lhs, rhs):
        assert op in self.operators
        self.op, self.lhs, self.rhs = op, lhs, rhs

class Assign(Expression):
    def __init__(self, name, expr):
        self.name, self.expr = name, expr

class Decl(object):
    def __init__(self, name, type_, init=None):
        assert type_ in CodeGen.type_map
        self.name, self.type, self.init = name, type_, init

class IfElse(Statement):
    def __init__(self, cond, then, else_):
        self.cond, self.then, self.else_ = cond, then, else_

class While(Statement):
    pass

class Return(Statement):
    def __init__(self, expression):
        self.expression = expression

class Func(object):
    def __init__(self, name, rettype, args, body):
        """
        args is a list of (argname, argtype). body is an instance of Block.
        """
        self.name = name
        self.rettype = rettype
        self.args = args
        self.body = body

class ExternFunc(object):
    def __init__(self, name, rettype, args):
        """
        args here is a list of argtypes.
        """
        self.name, self.rettype, self.args = name, rettype, args

class Scope(object):
    def __init__(self, name, llvmfunc, locals=None):
        self.name = name
        self.block = llvmfunc.append_basic_block(name)
        self.builder = Builder.new(self.block)
        self.locals = locals or {}

class CodeGen(object):
    type_map = {
        "int": Type.int(32),
        #"int16": Type.int(16),
        #"int32": Type.int(32),
        #"int64": Type.int(64),

        #"uint": Type.int(32),
        #"uint16": Type.int(16),
        "uint32": Type.int(32),
        "uint64": Type.int(64),

        "bool": Type.int(1),
    }

    def __init__(self, module_name):
        self.module = Module.new(module_name)
        self.scopes = []
        self.functions = {}
        self.current_func = None

    @classmethod
    def to_llvm_type(cls, typestr):
        if typestr not in cls.type_map:
            # for now, only POD types
            raise UnknownTypeError
        return cls.type_map[typestr]

    def emit(self, node):
        # TODO: pre-built dispatch table
        attr = "emit_" + camel2underscore(node.__class__.__name__)
        dispatch = getattr(self, attr, None)
        if not dispatch:
            raise UnknownNodeType(node.__class__.__name__)
        return dispatch(node)

    def emit_assign(self, node):
        scope = self.scopes[-1]
        return scope.builder.store(scope.locals[node.name],
                                   self.emit(node.expr))

    def emit_binary_expr(self, binexp):
        # TODO: make this less idiotic, possibly by using an op->lambda dict
        l, r = self.emit(binexp.lhs), self.emit(binexp.rhs)
        if binexp.op == "+":
            return self.scopes[-1].builder.add(l, r)
        return None

    def emit_block(self, statements):
        for stmt in statements:
            self.emit(stmt)

    def emit_decl(self, decl):
        scope = self.scopes[-1]
        if decl.name in scope.locals:
            return None
        var = scope.locals[decl.name] = \
            scope.builder.alloca(self.type_map[decl.type], decl.name)
        if decl.init is not None:
            scope.builder.store(decl.init, var)
        return var

    def emit_extern_func(self, extnode):
        assert extnode.name not in self.functions
        fntype = Type.function(self.type_map[extnode.rettype],
                               [self.type_map[t] for _, t in funcnode],
                               extnode.varargs)
        self.functions[extnode.name] = Function.new(self.module, fntype,
                                                    extnode.name)
        return self.funcnode[extnode.name]

    def emit_func(self, funcnode):
        assert funcnode.name not in self.functions
        fntype = Type.function(self.type_map[funcnode.rettype],
                               [self.type_map[t] for _, t in funcnode.args],
                               False)
        fn = Function.new(self.module, fntype, funcnode.name)
        s = Scope("", fn)

        for arg, (name, ty) in zip(fn.args, funcnode.args):
            arg.name = name
            var = s.locals[name] = s.builder.alloca(self.type_map[ty], name)
            s.builder.store(arg, var)

        self.scopes.append(s)
        self.functions[funcnode.name] = self.current_func = f
        self.emit_block(funcnode.body)
        self.scopes.pop()
        fn.verify()
        return f

    def emit_func_call(self, node):
        fn = self.functions.get(node.funcname)
        if fn is None:
            return None
        args = [self.emit(arg) for arg in node.args]
        return self.scopes[-1].builder.call(fn, args)

    def emit_if_else(self, node):
        self.emit(node.cond)
        s = Scope("", self.current_func)
        self.scopes.append(s)
        self.emit_block(node.then)

    def emit_int_literal(self, lit):
        v = lit.value
        return Constant.int(Type.int(max(v.bit_length(), 32)), v)

    def emit_type(self):
        pass

    def emit_variable(self, var):
        scope = self.scopes[-1]
        retval = scope.locals.get(var.name)
        if retval is None:
            return None
        return scope.builder.load(retval)

    def emit_while(self):
        pass

    def emit_return(self, ret):
        val = self.emit(ret.expression)
        return self.scopes[-1].builder.ret(val)

if __name__ == "__main__":
    """
    translation of:

    uint32 isqrt2(input uint32):
        base uint32 = 1 << 31
        result uint32 = 0
        while base > 0:
            nextapprox uint64 = result | base
            if nextapprox*nextapprox <= input:
                result = nextapprox as uint32
            base = base >> 1
        return result
    """

    '''
    Function('isqrt2', 'uint32', [('input', 'uint32')],
        Block([
            Decl('base', 'uint32',
                BinaryExpr('<<',
                    IntLiteral('1'),
                    IntLiteral('31')
                )
            ),
            Decl('result', 'uint32',
                IntLiteral('0')
            ),
            While(
                BinaryExpr('>',
                    Variable('base'),
                    IntLiteral('0')
                ),
                Block([
                    Decl('nextapprox', 'uint64',
                        BinaryExpr('|',
                            Variable('result'),
                            Variable('base')
                        )
                    ),
                    IfElse(
                        BinaryExpr('<=',
                            BinaryExpr('*',
                                Variable('nextapprox'),
                                Variable('nextapprox')
                            ),
                            Variable('input')
                        ),
                        Assign(
                            Variable('result'),
                            Cast('uint32',
                                Variable('nextapprox')
                            ),
                        )
                    ),
                    Assign(
                        Variable('base'),
                        BinaryExpr('>>',
                            Variable('base'),
                            IntLiteral('1')
                        )
                    )
                ])
            ),
            Return(Variable('result'))
        ])
    )
    '''

    f = Func('add_one', 'uint32', [('input', 'uint32')], [
        Return(
            BinaryExpr('+',
                Variable('input'),
                IntLiteral('1')
            )
        )
    ])

    cg = CodeGen('hellogravel')
    cg.emit(f)
    print cg.module
