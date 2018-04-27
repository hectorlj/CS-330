module RudInt

push!(LOAD_PATH, pwd())
using Error
using Lexer
export parse, calc, interp

#
# ==================================================
#

abstract type AE
end

# <AE> ::= <number>
type Num <: AE
    n::Real
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

# <AE> ::= (if0 <AE> <AE> <AE>)
type If0Node <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

type VarRefNode <: AE
    sym::Symbol
end

# <AE> ::= (<lambda> <id> <AE>)
type FuncDefNode <: AE
    formal::Symbol
    body::AE
end

# <AE> ::= (<AE> <AE>)
type FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE
end

# <AE> ::= (with <id> <AE> <AE>)
type WithNode <: AE
    sym::Symbol
    binding_expr::AE
    body::AE
end

function Dict(symbol)
    if symbol == :+
        return +
    elseif symbol == :-
        return -
    elseif symbol == :*
        return *
    elseif symbol == :/
        return /
    elseif symbol == :mod
        return mod
    elseif symbol == :collatz
        return collatz
    end
end

#
# ==================================================
#

abstract type RetVal
end

abstract type Environment
end

type NumVal <: RetVal
    n::Real
end

type ClosureVal <: RetVal
    formal::Symbol
    body::AE
    env::Environment
end

type EmptyEnv <: Environment
end

type ExtendedEnv <: Environment
    sym::Symbol
    val::RetVal
    parent::Environment
end


function parse( expr::Number )
    return Num( expr )
end

function parse( expr::Symbol)
    return VarRefNode( expr )
end

function parse( expr::Array{Any} )
    operator_symbol = expr[1];
    arg_num = size(expr)[1];
    if arg_num == 1
        try
            return parse(operator_symbol)
        catch
            throw(LispError("Needs to be an operator."))
        end
    elseif arg_num == 2
         if operator_symbol == :-
             operand = parse( expr[2])
         elseif operator_symbol == :collatz
             operand = parse( expr[2] )
         else

             throw(LispError("Invalid unary operation: $operator_symbol"))
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
        elseif operator == :lambda
            formal = parse(expr[2])
            body = parse(expr[3])
            return FuncDefNode(Dict(operator_symbol), formal, body)
        else
            throw(LispError("Unknown operator!"))
        end
        return Binop(Dict(operator_symbol), lhs, rhs)
    elseif arg_num == 4
        if operator_symbol == :with
            sym = parse(expr[2])
            binding_expr = parse(expr[3])
            body = parse(expr[4])
            return WithNode(Dict(operator_symbol), sym, binding_expr, body)
        elseif operator_symbol == :if0
            cond = parse(expr[2])
            zerobranch = parse(expr[3])
            nzerobranch = parse(expr[4])
            return If0Node((Dict(operator_symbol), cond, zerobranch, nzerobranch)
        else
            throw(LispError("Unknown operator!"))
        end
        throw(LispError("Invalid number of args for unary and/or binary operations, $arg_num arguments"))
    end
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# ==================================================
#

function calc( ast::Num )
    return ast.n
end

function calc( ast::Binop )
    current_op = ast.op
    if current_op == +
        return calc( ast.lhs ) + calc( ast.rhs )
    elseif current_op == -
        return calc( ast.lhs ) - calc( ast.rhs )
    elseif current_op == *
        return calc( ast.lhs ) * calc( ast.rhs )
    elseif current_op == /
        if ast.rhs.n == 0
            throw(LispError("Cannot divide by 0"))
        else
            return calc( ast.lhs ) / calc( ast.rhs )
        end
    elseif current_op == mod
        return mod(calc( ast.lhs ), calc( ast.rhs ))
    else
        throw(LispError("Invalid operator: $current_op"))
    end
end

function calc(ast::Unop)
    current_op = ast.op
    if current_op == -
        return clac(ast.operand)
    elseif current_op == collatz
        if calc(ast.operand) < 0
            throw(LispError("Negative numbers are not valid for collatz"))
        else
            return collatz(calc(ast.operand))
        end
    else
        throw(LispError("Invalid unary operator: $current_op"))
    end
end
#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
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

end #module
