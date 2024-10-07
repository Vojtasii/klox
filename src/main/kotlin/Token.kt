package cz.vojtasii.lox

data class Token(
    val type: TokenType,
    val lexeme: String,
    val literal: Any?,
    val line: Int,
) {
    override fun toString(): String =
        "$type $lexeme $literal"
}
