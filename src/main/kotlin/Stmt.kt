package cz.vojtasii.lox

sealed interface Stmt

interface StmtVisitor<R> {
    fun visit(stmt: Stmt): R
}

data class Block(val statements: List<Stmt>) : Stmt
data class Break(val keyword: Token) : Stmt
data class Class(val name: Token, val methods: List<Function>, val staticMethods: List<Function>): Stmt
data class Expression(val expression: Expr) : Stmt
data class Function(val name: Token, val params: List<Token>, val body: List<Stmt>) : Stmt
data class If(val condition: Expr, val thenBranch: Stmt, val elseBranch: Stmt?) : Stmt
data class Print(val expression: Expr) : Stmt
data class Return(val keyword: Token, val value: Expr?) : Stmt
data class Var(val name: Token, val initializer: Expr?) : Stmt
data class While(val condition: Expr, val body: Stmt) : Stmt

enum class FunctionType {
    NONE, FUNCTION, INITIALIZER, METHOD;

    override fun toString(): String = name.lowercase().replace('_',' ')
}

enum class ClassType {
    NONE, CLASS;
}

enum class LoopType {
    NONE, WHILE;
}
