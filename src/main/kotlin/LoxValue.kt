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
    val isInitializer: Boolean,
) : LoxValue, LoxCallable {

    constructor(declaration: Function, closure: Environment, isInitializer: Boolean = false) : this(
        declaration.name.lexeme, declaration.params, declaration.body, closure, isInitializer
    )

    constructor(declaration: AnonymousFunction, closure: Environment) : this(
        "(anonymous)", declaration.params, declaration.body, closure, false
    )

    override val arity: Int = params.size

    fun bind(instance: LoxInstance): LoxFunction {
        val environment = Environment(closure)
        environment.define("this", instance)
        return LoxFunction(name, params, body, environment, isInitializer)
    }

    override fun call(interpreter: Interpreter, arguments: List<LoxValue>): LoxValue {
        val environment = Environment(closure)
        require(params.size == arguments.size) {
            "Arguments and parameters have different arity."
        }
        params.zip(arguments).forEach { (param, arg) ->
            environment.define(param.lexeme, arg)
        }

        val returnValue = try {
            interpreter.executeBlock(body, environment)
            LoxNil
        } catch (returnJump: ReturnJump) {
            returnJump.value
        }

        return if (isInitializer) closure.getAt(0, "this") else returnValue
    }

    override fun toString(): String = "<fun $name>"
}

abstract class LoxNativeFun(override val arity: Int) : LoxValue, LoxCallable {
    override fun toString(): String = "<native fun>"
}

sealed interface LoxInstance : LoxValue {
    operator fun get(name: Token): LoxValue
    operator fun set(name: Token, value: LoxValue): LoxValue
}

data class LoxClass(
    val name: String,
    val methods: Map<String, LoxFunction>,
    val staticMethods: Map<String, LoxFunction>,
) : LoxValue, LoxCallable, LoxInstance {
    private val staticFields = mutableMapOf<String, LoxValue>()

    override val arity: Int
        get() = findMethod("init")?.arity ?: 0

    override fun call(interpreter: Interpreter, arguments: List<LoxValue>): LoxClassInstance {
        val instance = LoxClassInstance(this)
        val initializer = findMethod("init")
        initializer?.bind(instance)?.call(interpreter, arguments)

        return instance
    }

    fun findMethod(name: String): LoxFunction? = methods[name]

    override fun get(name: Token): LoxValue =
        staticFields[name.lexeme]
            ?: staticMethods[name.lexeme]?.bind(this)
            ?: throw RuntimeError(name, "Undefined property '${name.lexeme}'.")

    override fun set(name: Token, value: LoxValue): LoxValue {
        staticFields[name.lexeme] = value
        return value
    }

    override fun toString(): String = name
}

data class LoxClassInstance(val klass: LoxClass) : LoxValue, LoxInstance {
    private val fields = mutableMapOf<String, LoxValue>()

    override operator fun get(name: Token): LoxValue =
        fields[name.lexeme]
            ?: klass.findMethod(name.lexeme)?.bind(this)
            ?: throw RuntimeError(name, "Undefined property '${name.lexeme}'.")

    override operator fun set(name: Token, value: LoxValue): LoxValue {
        fields[name.lexeme] = value
        return value
    }

    override fun toString(): String = "${klass.name} instance"
}
