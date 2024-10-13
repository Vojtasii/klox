package cz.vojtasii.lox

class Environment {
    private val values = mutableMapOf<String, LoxValue>()

    fun define(name: String, value: LoxValue) {
        values[name] = value
    }

    fun assign(name: Token, value: LoxValue) =
        if (name.lexeme in values) {
            values[name.lexeme] = value
        } else {
            throw RuntimeError(name, "Undefined variable '${name.lexeme}'.")
        }

    operator fun get(name: Token): LoxValue =
        values.getOrElse(name.lexeme) {
            throw RuntimeError(name, "Undefined variable '${name.lexeme}'.")
        }
}
