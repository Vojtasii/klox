package cz.vojtasii.lox

class AstPrinter : ExprVisitor<String> {
    fun print(expr: Expr): String = expr.accept(this)

    override fun visitBinaryExpr(expr: Binary): String =
        parenthesize(expr.operator.lexeme, expr.left, expr.right)

    override fun visitGroupingExpr(expr: Grouping): String =
        parenthesize("group", expr.expression)

    override fun visitLiteralExpr(expr: Literal): String =
        expr.value?.toString() ?: "nil"

    override fun visitUnaryExpr(expr: Unary): String =
        parenthesize(expr.operator.lexeme, expr.right)

    private fun parenthesize(name: String, vararg exprs: Expr) = buildString {
        append('(')
        append(name)
        for (expr in exprs) {
            append(" ")
            append(expr.accept(this@AstPrinter))
        }
        append(")")
    }
}