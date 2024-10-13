package cz.vojtasii.lox

class Environment(
    val enclosing: Environment? = null,
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

    operator fun get(name: Token): LoxValue =
        values.getOrElse(name.lexeme) {
            if (enclosing != null) {
                enclosing[name]
            } else {
                throw RuntimeError(name, "Undefined variable '${name.lexeme}'.")
            }
        }
}
