package cz.vojtasii.lox

class Resolver(
    private val interpreter: Interpreter,
) : ExprVisitor<Unit>, StmtVisitor<Unit> {
    private val scopes = ArrayDeque<MutableMap<String, VarDef>>()
    private var currentFunction = FunctionType.NONE
    private var currentLoop = LoopType.NONE

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
            is Break -> if (currentLoop == LoopType.NONE) {
                Lox.error(stmt.keyword, "Can't break outside loop.")
            }
            is Class -> {
                declare(stmt.name)
                define(stmt.name)
            }
            is Expression -> visit(stmt.expression)
            is If -> {
                visit(stmt.condition)
                visit(stmt.thenBranch)
                if (stmt.elseBranch != null) visit(stmt.elseBranch)
            }
            is Print -> visit(stmt.expression)
            is Return -> {
                if (currentFunction == FunctionType.NONE) {
                    Lox.error(stmt.keyword, "Can't return from top-level code.")
                }

                if (stmt.value != null) visit(stmt.value)
            }
            is Function -> {
                declare(stmt.name)
                define(stmt.name)

                resolveFunction(stmt.params, stmt.body, FunctionType.FUNCTION)
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
                val enclosingLoop = currentLoop
                currentLoop = LoopType.WHILE
                visit(stmt.body)
                currentLoop = enclosingLoop
            }
        }
    }

    override fun visit(expr: Expr) {
        when (expr) {
            is AnonymousFunction -> resolveFunction(expr.params, expr.body, FunctionType.FUNCTION)
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
                if (scopes.lastOrNull()?.get(expr.name.lexeme)?.state == VarState.DECLARED) {
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
        val scope = scopes.removeLast()
        scope.filter { it.value.state != VarState.USED }.forEach { (_, unusedVariable) ->
            Lox.error(unusedVariable.name, "Unused local variable.")
        }
    }

    private fun declare(name: Token) {
        val scope = scopes.lastOrNull() ?: return
        if (scope.containsKey(name.lexeme)) {
            Lox.error(name, "Already a variable with this name in this scope.")
        }

        scope[name.lexeme] = VarDef(name, VarState.DECLARED)
    }

    private fun define(name: Token) {
        val scope = scopes.lastOrNull() ?: return
        scope[name.lexeme] = VarDef(name, VarState.DEFINED)
    }

    private fun resolveFunction(params: List<Token>, body: List<Stmt>, type: FunctionType) {
        val enclosingFunction = currentFunction
        currentFunction = type

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
        val (index, scope) = scopes.withIndex().lastOrNull { (_, scope) ->
            scope.containsKey(name.lexeme)
        } ?: return

        scope.computeIfPresent(name.lexeme) { _, variable ->
            variable.copy(state = VarState.USED)
        }

        interpreter.resolve(expr, scopes.size - 1 - index)
    }

    private data class VarDef(val name: Token, val state: VarState)

    private enum class VarState {
        DECLARED, DEFINED, USED;
    }
}