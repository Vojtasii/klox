package cz.vojtasii.lox

class Resolver(
    private val interpreter: Interpreter,
) : ExprVisitor<Unit>, StmtVisitor<Unit> {
    private val scopes = ArrayDeque<MutableMap<String, Boolean>>()
    private var currentFunction = Function.Kind.NONE

    fun visit(statements: List<Stmt>) {
        statements.forEach(this::visit)
    }

    override fun visit(stmt: Stmt) {
        when (stmt) {
            is Block -> {
                beginScope()
                visit(stmt.statements)
                endScope()
            }
            Break -> Unit
            is Expression -> visit(stmt.expression)
            is If -> {
                visit(stmt.condition)
                visit(stmt.thenBranch)
                if (stmt.elseBranch != null) visit(stmt.elseBranch)
            }
            is Print -> visit(stmt.expression)
            is Return -> {
                if (currentFunction == Function.Kind.NONE) {
                    Lox.error(stmt.keyword, "Can't return from top-level code.")
                }

                if (stmt.value != null) visit(stmt.value)
            }
            is Function -> {
                declare(stmt.name)
                define(stmt.name)

                resolveFunction(stmt.params, stmt.body, Function.Kind.FUNCTION)
            }
            is Var -> {
                declare(stmt.name)
                if (stmt.initializer != null) {
                    visit(stmt.initializer)
                }
                define(stmt.name)
            }
            is While -> {
                visit(stmt.condition)
                visit(stmt.body)
            }
        }
    }

    override fun visit(expr: Expr) {
        when (expr) {
            is AnonymousFunction -> resolveFunction(expr.params, expr.body, Function.Kind.FUNCTION)
            is Assign -> {
                visit(expr.value)
                resolveLocal(expr, expr.name)
            }
            is Binary -> {
                visit(expr.left)
                visit(expr.right)
            }
            is Call -> {
                visit(expr.callee)
                expr.arguments.forEach(::visit)
            }
            is Grouping -> visit(expr.expression)
            is Literal -> Unit
            is Logical -> {
                visit(expr.left)
                visit(expr.right)
            }
            is TernaryConditional -> {
                visit(expr.condition)
                visit(expr.truly)
                visit(expr.falsy)
            }
            is Unary -> visit(expr.right)
            is Variable -> {
                if (scopes.lastOrNull()?.get(expr.name.lexeme) == false) {
                    Lox.error(expr.name, "Can't read local variable in its own initializer.")
                }

                resolveLocal(expr, expr.name)
            }
        }
    }

    private fun beginScope() {
        scopes.add(mutableMapOf())
    }

    private fun endScope() {
        scopes.removeLast()
    }

    private fun declare(name: Token) {
        val scope = scopes.lastOrNull() ?: return
        if (scope.containsKey(name.lexeme)) {
            Lox.error(name, "Already a variable with this name in this scope.")
        }

        scope[name.lexeme] = false
    }

    private fun define(name: Token) {
        scopes.lastOrNull()?.put(name.lexeme, true)
    }

    private fun resolveFunction(params: List<Token>, body: List<Stmt>, kind: Function.Kind) {
        val enclosingFunction = currentFunction
        currentFunction = kind

        beginScope()
        for (param in params) {
            declare(param)
            define(param)
        }
        visit(body)
        endScope()
        currentFunction = enclosingFunction
    }

    private fun resolveLocal(expr: Expr, name: Token) {
        val index = scopes.withIndex().lastOrNull { (_, scope) ->
            scope.containsKey(name.lexeme)
        }?.index ?: return

        interpreter.resolve(expr, scopes.size - 1 - index)
    }
}