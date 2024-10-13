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
    val declaration: Function,
    val closure: Environment,
) : LoxValue, LoxCallable {
    override val arity: Int = declaration.params.size

    override fun call(interpreter: Interpreter, arguments: List<LoxValue>): LoxValue {
        val environment = Environment(closure)
        require(declaration.params.size == arguments.size) {
            "Arguments and parameters have different arity."
        }
        declaration.params.zip(arguments).forEach { (param, arg) ->
            environment.define(param.lexeme, arg)
        }

        return try {
            interpreter.executeBlock(declaration.body, environment)
            LoxNil
        } catch (returnJump: ReturnJump) {
            returnJump.value
        }
    }

    override fun toString(): String = "<fun ${declaration.name.lexeme}>"
}

abstract class LoxNativeFun(override val arity: Int) : LoxValue, LoxCallable {
    override fun toString(): String = "<native fun>"
}
