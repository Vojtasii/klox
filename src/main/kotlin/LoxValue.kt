package cz.vojtasii.lox

sealed interface LoxValue

data object LoxNil : LoxValue {
    override fun toString(): String = "nil"
}

@JvmInline
value class LoxBoolean(val value: Boolean) : LoxValue {
    override fun toString(): String = value.toString()
}

@JvmInline
value class LoxNumber(val value: Double) : LoxValue {
    override fun toString(): String = value.toString()
}

@JvmInline
value class LoxString(val value: String) : LoxValue {
    override fun toString(): String = value
}
