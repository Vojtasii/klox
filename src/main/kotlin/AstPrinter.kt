package cz.vojtasii.lox

object AstPrinter : ExprVisitor<String> {

    override fun visit(expr: Expr): String = when (expr) {
        is Binary -> parenthesize(expr.operator.lexeme, expr.left, expr.right)
        is Grouping -> parenthesize("group", expr.expression)
        is Literal -> expr.value?.toString() ?: "nil"
        is Unary -> parenthesize(expr.operator.lexeme, expr.right)
    }

    private fun parenthesize(name: String, vararg exprs: Expr) = buildString {
        append('(')
        append(name)
        for (expr in exprs) {
            append(" ")
            append(visit(expr))
        }
        append(")")
    }
}