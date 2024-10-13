package cz.vojtasii.lox

sealed interface Expr

interface ExprVisitor<R> {
    fun visit(expr: Expr): R
}

data class Binary(val left: Expr, val operator: Token, val right: Expr) : Expr
data class Grouping(val expression: Expr) : Expr
data class Literal(val value: LoxValue) : Expr
data class Logical(val left: Expr, val operator: Token, val right: Expr) : Expr
data class Unary(val operator: Token, val right: Expr) : Expr
data class Variable(val name: Token) : Expr
data class Assign(val name: Token, val value: Expr) : Expr
data class TernaryConditional(val condition: Expr, val truly: Expr, val falsy: Expr) : Expr
