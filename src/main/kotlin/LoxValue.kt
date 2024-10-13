package cz.vojtasii.lox

sealed interface LoxValue {
    val isTruthy: Boolean
        get() = when (this) {
            is LoxNil -> false
            is LoxBoolean -> value
            else -> true
        }
}

interface LoxCallable {
    val arity: Int
    fun call(interpreter: Interpreter, arguments: List<LoxValue>): LoxValue
}

data object LoxNil : LoxValue {
    override fun toString(): String = "nil"
}

@JvmInline
value class LoxBoolean(val value: Boolean) : LoxValue {
    override fun toString(): String = value.toString()
}

@JvmInline
value class LoxNumber(val value: Double) : LoxValue {
    override fun toString(): String = value.toString().removeSuffix(".0")
}

@JvmInline
value class LoxString(val value: String) : LoxValue {
    override fun toString(): String = value
}

data class LoxFunction(
    val name: String,
    val params: List<Token>,
    val body: List<Stmt>,
    val closure: Environment,
) : LoxValue, LoxCallable {

    constructor(declaration: Function, closure: Environment) : this(
        declaration.name.lexeme, declaration.params, declaration.body, closure
    )

    constructor(declaration: AnonymousFunction, closure: Environment) : this(
        "(anonymous)", declaration.params, declaration.body, closure
    )

    override val arity: Int = params.size

    override fun call(interpreter: Interpreter, arguments: List<LoxValue>): LoxValue {
        val environment = Environment(closure)
        require(params.size == arguments.size) {
            "Arguments and parameters have different arity."
        }
        params.zip(arguments).forEach { (param, arg) ->
            environment.define(param.lexeme, arg)
        }

        return try {
            interpreter.executeBlock(body, environment)
            LoxNil
        } catch (returnJump: ReturnJump) {
            returnJump.value
        }
    }

    override fun toString(): String = "<fun $name>"
}

abstract class LoxNativeFun(override val arity: Int) : LoxValue, LoxCallable {
    override fun toString(): String = "<native fun>"
}
