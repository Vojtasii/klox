package cz.vojtasii.lox

sealed interface Stmt

interface StmtVisitor<R> {
    fun visit(stmt: Stmt): R
}

data class Expression(val expression: Expr): Stmt

data class Print(val expression: Expr): Stmt

data class Var(val name: Token, val initializer: Expr?): Stmt
