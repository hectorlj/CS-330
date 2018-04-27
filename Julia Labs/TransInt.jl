module TransInt

push!(LOAD_PATH, pwd())
using Error
using Lexer
export parse, calc, analyze, NumVal, ClosureVal

#
# ==================================================
#

abstract type AE end
abstract type Environment end
abstract type RetVal end

# <AE> ::= <number>
type Num <: AE
    n::Real
end

type NumVal <: RetVal
    n::Real
end

type EmptyEnv <: Environment
end

type SymVal <: AE
    name::Symbol
    value::RetVal
end

type ExtendedEnv <: Environment
    symvals::Array{SymVal}
    parent::Environment
end

type Plus <: AE
    args::Array{AE}
end

type And <: AE
    args::Array{AE}
end

type ClosureVal <: RetVal
    param::Array{Symbol}
    body::AE
    env::Environment
end

type Binop <: AE
    op::Function
    lhs::AE
    rhs::AE
end
type Unop <: AE
    op::Function
    operand::AE
end

type If0 <: AE
    condition::AE
    zero_branch::AE
    nonzero_branch::AE
end

type Binder <: AE
    name::Symbol
    binding_exprs::AE
end

type With <: AE
    binders::Array{Binder}
    body::AE
end

type Id <: AE
    name::Symbol
end

type FuncDef <: AE
    formal_parameters::Array{Symbol}
    fun_body::AE
end

type FuncApp <: AE
    func_expr::AE
    arg_exprs::Array{AE}
end

type VarRefNode <: AE
    sym::Symbol
end

function collatz( n::Real )
  return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

# function Dict(symbol)
#     if symbol == :+
#         return +
#     elseif symbol == :-
#         return -
#     elseif symbol == :*
#         return *
#     elseif symbol == :/
#         return /
#     elseif symbol == :mod
#         return mod
#     elseif symbol == :collatz
#         return collatz
#     end
# end
#
#
# ==================================================
#

function no_dups(exprs::Array{Any})
    if length(exprs) == 0
        return true
    end
    syms = Any[]
    if typeof(exprs[1]) == Array{Any, 1}
        for i in 1:size(exprs, 1)
            if in(exprs[i][1], syms)
                return false
            else
                push!(syms, exprs[i][1])
            end
        end
        return true
    elseif typeof(exprs[1]) == Symbol
        for i in 1:size(exprs, 1)
            if typeof(exprs[i]) != Symbol
                throw(LispError("Invalid symbols"))
            end
            if in(exprs[i], syms)
                return false
            else
                push!(syms, exprs[i])
            end
        end
        return true
    else
        throw(LispError("Missing stuff"))
    end
end

function no_dups(exprs::Any)
    throw(LispError("Improper format"))
end

function getBinders(exprs::Array{Any})
    binders = Binder[]
    for i in 1:size(exprs, 1)
        push!(binders, Binder(exprs[i][1], parse(exprs[i][2])))
    end
    return binders
end


function hasSymVal(symvals::Array{SymVal}, name::Symbol)
    for i in 1:size(symvals, 1)
        if symvals[i].name == name
            return true
        end
    end
    return false
end

function getValue(symvals::Array{SymVal}, name::Symbol)
    for i in 1:size(symvals, 1)
        if symvals[i].name == name
            return symvals[i].value
        end
    end
    throw(LispError("Fatal Error"))
end

function parse( expr::Number )
    return Num( expr )
end

function parse(expr::Bool)
    return Bool(expr)
end

function parse(expr::Symbol)
    if expr == :- || expr == :+ || expr == :* || expr == :/ || expr ==:mod || expr == :collatz || expr == :if0 || expr == :with || expr == :lambda
        throw(LispError("Operators cannot be Ids"))
    else
        return Id(expr)
    end
end

binop = Dict(
    :- => -,
    :* => *,
    :/ => /,
    :mod => mod 
)

unop = Dict(
    :- => -,
    :collatz => function col(x,y)
    if y <= 0
        throw(error)
    end
    return collatz(y, x)
end
    )

reserved = [:+, :-, :*, :/,  :mod, :collatz, :if0, :with, :lambda, :and]

function parse( expr::Array{Any} )
    operator_symbol = expr[1]
    arg_num = size(expr)[1]

    if arg_num == 1
        return parse(expr[1])
    end
    if operator_symbol == :+
        if length(expr) < 3
            throw(LispError("Not enough"))
        end
        return Plus(map(x->parse(x), expr[2:end]))
    end
    if operator_symbol == :and
        if length(expr) < 3
            throw(LispError("Not Enough"))
        end
        return And(map(x->parse(x), expr[2:end]))
    elseif operator_symbol == :- || operator_symbol == :+ || operator_symbol == :* || operator_symbol == :/ || operator_symbol ==:mod || operator_symbol == :collatz
        if arg_num == 2
            if operator_symbol == :-
                operand = parse( expr[2])
            elseif operator_symbol == :collatz
                operand = parse( expr[2] )
            else
                throw(LispError("Invalid unary operation: "))
            end
            return Unop(Dict(operator_symbol), operand)
        elseif arg_num == 3
           if operator_symbol == :+
               lhs = parse(expr[2])
               rhs = parse(expr[3])
           elseif operator_symbol == :-
               lhs = parse(expr[2])
               rhs = parse(expr[3])
           elseif operator_symbol == :*
               lhs = parse(expr[2])
               rhs = parse(expr[3])
           elseif operator_symbol == :/
               lhs = parse(expr[2])
               rhs = parse(expr[3])
           elseif operator_symbol == :mod
               lhs = parse(expr[2])
               rhs = parse(expr[3])
           else
               throw(LispError("Invalid binary operation: "))
           end
           return Binop(Dict(operator_symbol), lhs, rhs)
       end
    elseif operator_symbol == :if0
        if length(expr) == 4
            condition = parse(expr[2])
            zero_branch = parse(expr[3])
            nonzero_branch = parse(expr[4])
            return If0(condition, zero_branch, nonzero_branch)
        else
            throw(LispError("invalid if0 syntax"))
        end
    elseif operator_symbol == :with
        if length(expr) != 3
            throw(LispError("invalid with syntax"))
        end
        binding_exprs = expr[2]
        if no_dups(binding_exprs)
            binders = getBinders(binding_exprs)
            body = parse(expr[3])
            return With(binders, body)
        else
            throw(LispError("duplicate Symbols in with"))
        end
    elseif operator_symbol == :lambda
        if length(expr) != 3
            throw(LispError("Invalid lambda syntax"))
        end
        if no_dups(expr[2])
            return FuncDef(expr[2], parse(expr[3]))
        else
            throw(LispError("Duplicate symbols in lambda"))
        end
    else
        func_expr = parse(expr[1])
        arg_exprs = AE[]
        for i in 2:size(expr, 1)
            push!(arg_exprs, parse(expr[i]))
        end
        return FuncApp(func_expr, arg_exprs)
    end
end


function parse( expr::Any )
  throw( LispError("Invalid type ") )
end

#
# ==================================================
#

function analyze(ast::AE)
    return ast
end

function analyze(ast::VarRefNode)
    return ast
end

function analyze(ast::NumVal)
    return ast    
end

function analyze(ast::Unop)
    return Unop(ast.op, analyze(ast.operand))
end

function analyze(ast::Binop)
    return Binop(ast.op, analyze(ast.lhs), analyze(ast.rhs))
end

function analyze(ast::If0)
    return If0(analyze(ast.condition), analyze(ast.zero_branch), analyze(ast.nonzero_branch))
end

function analyze(ast::Plus)
    return Plus(map(x-> analyze(x)), And.args)
end

function analyze(ast::And)
    return And(map(x-> analyze(x)), And.args)
end
# (With((x 1) (y 2)) (+ x y)) ===> ((lambda (x y) (+ x y )) 1 2) ==> 3
# [:x, :y]
function analyze(ast::With)
    formals = map(x-> x.name, ast.binders) 
    args = map(x-> analyze(x.binding_exprs), ast.binders)
    return FuncApp(FuncDef(formals, analyze(ast.body)), args)
end

function analyze(ast::FuncDef)
    return FuncDef(ast.formal_parameters, analyze(ast.fun_body))
end

function analyze(ast::FuncApp)
    return FuncApp(analyze(ast.func_expr), map(x-> analyze(x), ast.arg_exprs))
end

#
# ==================================================
#

function calc(ast::AE)
    return calc(ast, EmptyEnv())
end

function calc(num::Num, env::Environment)
    return NumVal(num.n)
end

function calc( ast::Binop, env::Environment )
    left = calc(ast.lhs, env)
    right = calc(ast.rhs, env)
    if typeof(left) == NumVal && typeof(right) == NumVal
        if (ast.op == /) && right.n == 0
            throw(LispError("cannot Divide by 0"))
        else
            return NumVal(ast.op(left.n, right.n))
        end
    else
        throw(LispError("Invalid binary types"))
    end
end


function calc(ast::Unop, env::Environment)
    unary = calc(ast.operand, env)
    if typeof(unary) == NumVal
        if ast.op == collatz && unary.n <= 0
            throw(LispError("invalid expression for collatz"))
        else
            return NumVal(ast.op(unary.n))
        end
    else
        throw(LispError("Invalid unary types"))
    end
end

function calc(ast::If0, env::Environment)
    cond = calc(ast.condition, env)
    if typeof(cond) == NumVal
        if cond.n == 0
            return calc(ast.zero_branch, env)
        else
            return calc(ast.nonzero_branch, env)
        end
    else
        throw(LispError("Error in value Condition "))
    end
end

function calc(ast::Id, env::Environment)
    if typeof(env) == EmptyEnv
        throw(LispError("could not find symbol "))
    elseif hasSymVal(env.symvals, ast.name)
        return getValue(env.symvals, ast.name)
    else
        return calc(ast, env.parent)
    end
end


# function calc(ast::With, env::Environment)
#     symvals = SymVal[]
#     for i in 1:size(ast.binders, 1)
#         push!(symvals, SymVal(ast.binders[i].name, calc(ast.binders[i].binding_exprs, env)))
#     end
#     extended_env = ExtendedEnv(symvals, env)
#     return calc(ast.body, extended_env)
# end

function calc(ast::FuncApp, env::Environment)
    closure = calc(ast.func_expr, env)
    if typeof(closure) == ClosureVal
        if length(ast.arg_exprs) != length(closure.param)
            throw(LispError("parameter number and function call do not match"))
        end
        symvals = SymVal[]
        for i in 1:size(ast.arg_exprs, 1)
            push!(symvals, SymVal(closure.param[i], calc(ast.arg_exprs[i], env)))
        end
        new_env = ExtendedEnv(symvals, closure.env)
        rval = calc(closure.body, new_env)
        return rval
    else
        throw(LispError("ClosureVal not returned"))
    end
end

function calc( ast::FuncDef, env::Environment)
    if size(ast.formal_parameters)[1] == 0
        return calc(ast.fun_body, env)
    end
    return ClosureVal(ast.formal_parameters, ast.fun_body, env)
end

function calc(failure::Any)
    throw(LispError("Syntax Error: Invalid operation or id "))
end

function calc(failure::Any, env::Environment)
    throw(LispError("Error,"))
end


#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    ast = analyze( ast )
    return calc( ast, EmptyEnv() )
end

end #module
