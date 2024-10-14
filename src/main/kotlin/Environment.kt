package cz.vojtasii.lox

class Environment(
    private val enclosing: Environment? = null,
) {
    private val values = mutableMapOf<String, LoxValue>()

    fun define(name: String, value: LoxValue) {
        values[name] = value
    }

    fun assign(name: Token, value: LoxValue): Unit =
        if (name.lexeme in values) {
            values[name.lexeme] = value
        } else if (enclosing != null) {
            enclosing.assign(name, value)
        } else {
            throw RuntimeError(name, "Undefined variable '${name.lexeme}'.")
        }

    fun assignAt(distance: Int, name: Token, value: LoxValue) {
        ancestor(distance).values.put(name.lexeme, value)
    }

    operator fun get(name: Token): LoxValue =
        values.getOrElse(name.lexeme) {
            if (enclosing != null) {
                enclosing[name]
            } else {
                throw RuntimeError(name, "Undefined variable '${name.lexeme}'.")
            }
        }

    fun getAt(distance: Int, name: String): LoxValue =
        ancestor(distance).values.getValue(name)

    private fun ancestor(distance: Int): Environment {
        var environment = this
        for (i in 0..<distance) {
            environment = requireNotNull(environment.enclosing)
        }

        return environment
    }
}
