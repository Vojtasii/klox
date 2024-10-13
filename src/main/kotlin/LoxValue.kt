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

abstract class LoxNativeFun(override val arity: Int): LoxValue, LoxCallable {
    override fun toString(): String = "<native fun>"
}
