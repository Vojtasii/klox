package cz.vojtasii.lox

object AstPrinter : ExprVisitor<String> {

    override fun visit(expr: Expr): String = when (expr) {
        is Binary -> parenthesize(expr.operator.lexeme, expr.left, expr.right)
        is AnonymousFunction -> {
            val params = expr.params.joinToString(prefix = "(", postfix = ")") { it.lexeme }
            parenthesize("fun $params")
        }
        is Call -> parenthesize(expr.callee.toString(), *expr.arguments.toTypedArray())
        is Grouping -> parenthesize("group", expr.expression)
        is Literal -> expr.value.toString()
        is Logical -> parenthesize(expr.operator.lexeme, expr.left, expr.right)
        is Unary -> parenthesize(expr.operator.lexeme, expr.right)
        is Variable -> expr.name.lexeme
        is Assign -> parenthesize("let ${expr.name.lexeme}", expr.value)
        is TernaryConditional -> parenthesize("cond", expr.condition, expr.truly, expr.falsy)
        is Get -> parenthesize("get .${expr.name.lexeme}", expr.obj)
        is Set -> parenthesize("set .${expr.name.lexeme}", expr.obj, expr.value)
        is This -> expr.keyword.lexeme
        is Super -> parenthesize("${expr.keyword.lexeme} .${expr.method.lexeme}")
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