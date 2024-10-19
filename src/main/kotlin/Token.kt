package cz.vojtasii.lox

data class Token(
    val type: TokenType,
    val lexeme: String,
    val literal: LoxValue?,
    val line: Int,
    val column: Int,
) {
    override fun toString(): String = "$type $lexeme $literal"
}
