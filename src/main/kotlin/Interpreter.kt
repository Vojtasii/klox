package cz.vojtasii.lox

import java.util.IdentityHashMap

class Interpreter : ExprVisitor<LoxValue>, StmtVisitor<Unit> {
    private val globals: Environment = Environment().apply {
        define(
            "clock",
            object : LoxNativeFun(0) {
                override fun call(interpreter: Interpreter, arguments: List<LoxValue>): LoxNumber =
                    LoxNumber(System.currentTimeMillis() / 1000.0)
            },
        )
    }
    private var environment: Environment = globals
    private val locals: IdentityHashMap<Expr, Int> = IdentityHashMap()

    fun interpret(statements: List<Stmt>) {
        try {
            for (statement in statements) {
                execute(statement)
            }
        } catch (error: RuntimeError) {
            Lox.runtimeError(error)
        }
    }

    private fun execute(statement: Stmt) {
        visit(statement)
    }

    fun resolve(expr: Expr, depth: Int) {
        locals[expr] = depth
    }

    fun executeBlock(
        statements: List<Stmt>,
        environment: Environment,
    ) {
        val previous = this.environment
        try {
            this.environment = environment

            for (statement in statements) {
                execute(statement)
            }
        } finally {
            this.environment = previous
        }
    }

    override fun visit(stmt: Stmt) {
        when (stmt) {
            is Block -> executeBlock(stmt.statements, Environment(environment))
            is Break -> throw BreakJump()
            is Class -> {
                environment.define(stmt.name.lexeme, LoxNil)

                val methods = stmt.methods.associate { method ->
                    val function = LoxFunction(method, environment, method.name.lexeme == "init")
                    method.name.lexeme to function
                }
                val staticMethods = stmt.staticMethods.associate { staticMethod ->
                    val function = LoxFunction(staticMethod, environment)
                    staticMethod.name.lexeme to function
                }

                val klass = LoxClass(stmt.name.lexeme, methods, staticMethods)
                environment.assign(stmt.name, klass)
            }
            is Expression -> visit(stmt.expression)
            is Function -> {
                val function = LoxFunction(stmt, environment)
                environment.define(stmt.name.lexeme, function)
            }
            is If -> if (visit(stmt.condition).isTruthy) {
                execute(stmt.thenBranch)
            } else if (stmt.elseBranch != null) {
                execute(stmt.elseBranch)
            }
            is Print -> {
                val value = visit(stmt.expression)
                println(value)
            }
            is Return -> {
                val value = stmt.value?.let { visit(it) } ?: LoxNil
                throw ReturnJump(value)
            }
            is Var -> {
                val value = stmt.initializer?.let { visit(it) } ?: LoxNil
                environment.define(stmt.name.lexeme, value)
            }
            is While -> try {
                while (visit(stmt.condition).isTruthy) {
                    execute(stmt.body)
                }
            } catch (_: BreakJump) {
            }
        }
    }

    override fun visit(expr: Expr): LoxValue =
        when (expr) {
            is AnonymousFunction -> LoxFunction(expr, environment)
            is Binary -> evaluateBinaryExpr(expr)
            is Call -> evaluateCallExpr(expr)
            is Get -> evaluateGetExpr(expr)
            is Grouping -> visit(expr.expression)
            is Literal -> expr.value
            is Logical -> evaluateLogicalExpr(expr)
            is Set -> evaluateSetExpr(expr)
            is This -> lookUpVariable(expr.keyword, expr)
            is TernaryConditional -> evaluateTernaryCondExpr(expr)
            is Unary -> evaluateUnaryExpr(expr)
            is Variable -> lookUpVariable(expr.name, expr)
            is Assign -> visit(expr.value).also { value ->
                val distance = locals[expr]
                if (distance != null) {
                    environment.assignAt(distance, expr.name, value)
                } else {
                    globals.assign(expr.name, value)
                }
            }
        }

    private fun evaluateLogicalExpr(expr: Logical): LoxValue {
        val left = visit(expr.left)

        return when (expr.operator.type) {
            TokenType.OR -> if (left.isTruthy) left else visit(expr.right)
            TokenType.AND -> if (!left.isTruthy) left else visit(expr.right)
            else -> throw RuntimeError(expr.operator, "Unexpected logical operator.")
        }
    }

    private fun evaluateUnaryExpr(expr: Unary): LoxValue {
        val right = visit(expr.right)

        return when (expr.operator.type) {
            TokenType.BANG -> LoxBoolean(!right.isTruthy)
            TokenType.MINUS -> expectNumber(expr.operator, right) { r ->
                LoxNumber(-r.value)
            }
            else -> throw RuntimeError(expr.operator, "Unexpected unary operator.")
        }
    }

    private fun evaluateBinaryExpr(expr: Binary): LoxValue {
        val left = visit(expr.left)
        val right = visit(expr.right)

        return when (expr.operator.type) {
            TokenType.MINUS -> expectNumbers(expr.operator, left, right) { l, r ->
                LoxNumber(l.value - r.value)
            }
            TokenType.SLASH -> expectNumbers(expr.operator, left, right) { l, r ->
                LoxNumber(l.value / r.value)
            }
            TokenType.STAR -> expectNumbers(expr.operator, left, right) { l, r ->
                LoxNumber(l.value * r.value)
            }
            TokenType.PLUS -> when {
                left is LoxNumber && right is LoxNumber -> LoxNumber(left.value + right.value)
                left is LoxString -> LoxString(left.value + right.toString())
                right is LoxString -> LoxString(left.toString() + right.value)
                else -> throw RuntimeError(expr.operator, "Expect numbers or strings")
            }
            TokenType.GREATER -> expectNumbers(expr.operator, left, right) { l, r ->
                LoxBoolean(l.value > r.value)
            }
            TokenType.GREATER_EQUAL -> expectNumbers(expr.operator, left, right) { l, r ->
                LoxBoolean(l.value >= r.value)
            }
            TokenType.LESS -> expectNumbers(expr.operator, left, right) { l, r ->
                LoxBoolean(l.value < r.value)
            }
            TokenType.LESS_EQUAL -> expectNumbers(expr.operator, left, right) { l, r ->
                LoxBoolean(l.value <= r.value)
            }
            TokenType.BANG_EQUAL -> LoxBoolean(left != right)
            TokenType.EQUAL_EQUAL -> LoxBoolean(left == right)
            TokenType.COMMA -> right
            else -> throw RuntimeError(expr.operator, "Unexpected binary operator.")
        }
    }

    private fun evaluateCallExpr(expr: Call): LoxValue {
        val callee = visit(expr.callee)

        val arguments = expr.arguments.map(::visit)

        val callable = callee as? LoxCallable
            ?: throw RuntimeError(expr.paren, "Can only call functions and classes.")
        if (arguments.size != callable.arity) {
            throw RuntimeError(expr.paren, "Expected ${callable.arity} arguments, but got ${arguments.size}.")
        }
        return callable.call(this, arguments)
    }

    private fun evaluateGetExpr(expr: Get): LoxValue {
        return when (val obj = visit(expr.obj)) {
            is LoxInstance -> obj[expr.name]
            else -> throw RuntimeError(expr.name, "Only instances have properties.")
        }
    }

    private fun evaluateSetExpr(expr: Set): LoxValue {
        return when (val obj = visit(expr.obj)) {
            is LoxInstance -> {
                val value = visit(expr.value)
                obj.set(expr.name, value)
            }
            else -> throw RuntimeError(expr.name, "Only instances have fields.")
        }
    }

    private fun lookUpVariable(name: Token, expr: Expr): LoxValue {
        val distance = locals[expr]
        return if (distance != null) {
            environment.getAt(distance, name.lexeme)
        } else {
            globals[name]
        }
    }

    private fun evaluateTernaryCondExpr(expr: TernaryConditional): LoxValue {
        val condition = visit(expr.condition)

        return if (condition.isTruthy) {
            visit(expr.truly)
        } else {
            visit(expr.falsy)
        }
    }

    private inline fun expectNumber(
        operator: Token,
        operand: LoxValue,
        block: (LoxNumber) -> LoxValue,
    ): LoxValue = when (operand) {
        is LoxNumber -> block(operand)
        else -> throw RuntimeError(operator, "Operand must be a number.")
    }

    private inline fun expectNumbers(
        operator: Token,
        left: LoxValue,
        right: LoxValue,
        block: (LoxNumber, LoxNumber) -> LoxValue,
    ): LoxValue = when {
        left is LoxNumber && right is LoxNumber -> block(left, right)
        else -> throw RuntimeError(operator, "Operands must be numbers.")
    }
}
