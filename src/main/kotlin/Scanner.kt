package cz.vojtasii.lox

class Scanner(
    private val source: String,
) {
    private val tokens: MutableList<Token> = mutableListOf()
    private var start = 0
    private var current = 0
    private var line = 1
    private var lineStart = 0

    fun scanTokens(): List<Token> {
        while (!isAtEnd()) {
            // We are at the beginning of the next lexeme.
            start = current
            scanToken()
        }

        tokens.add(Token(TokenType.EOF, "", null, line, start - lineStart + 1))
        return tokens
    }

    private fun scanToken() {
        return when (val c = advance()) {
            '(' -> addToken(TokenType.LEFT_PAREN)
            ')' -> addToken(TokenType.RIGHT_PAREN)
            '{' -> addToken(TokenType.LEFT_BRACE)
            '}' -> addToken(TokenType.RIGHT_BRACE)
            ',' -> addToken(TokenType.COMMA)
            '.' -> addToken(TokenType.DOT)
            '-' -> addToken(TokenType.MINUS)
            '+' -> addToken(TokenType.PLUS)
            ';' -> addToken(TokenType.SEMICOLON)
            '*' -> addToken(TokenType.STAR)
            '?' -> addToken(TokenType.QUESTION)
            ':' -> addToken(TokenType.COLON)
            '!' -> addToken(
                if (match('=')) TokenType.BANG_EQUAL else TokenType.BANG
            )
            '=' -> addToken(
                if (match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL
            )
            '<' -> addToken(
                if (match('=')) TokenType.LESS_EQUAL else TokenType.LESS
            )
            '>' -> addToken(
                if (match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER
            )
            '/' -> if (match('/')) {
                // A comment goes until the end of the line.
                while (peek() != '\n' && !isAtEnd()) advance()
            } else if (match('*')) {
                // A comment goes until finding a matching `*/`
                while (peek() != '*' && peekNext() != '/' && !isAtEnd()) advance()
                if (!isAtEnd()) {
                    // The closing `*` and `/`
                    advance()
                    advance()
                }
                Unit
            } else {
                addToken(TokenType.SLASH)
            }
            ' ', '\r', '\t' -> Unit // Ignore whitespace
            '\n' -> newLine()
            '"' -> string()
            else -> when {
                c.isDigit() -> number()
                c.isAlpha() -> identifier()
                else -> Lox.error(line, start - lineStart, "Unexpected character.")
            }
        }
    }

    private fun identifier() {
        while (peek().isAlphaNumeric()) advance()
        val text = source.substring(start, current)
        val type = keywords.getOrDefault(text, TokenType.IDENTIFIER)
        addToken(type)
    }

    private fun string() {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n') newLine()
            advance()
        }

        if (isAtEnd()) {
            Lox.error(line, start - lineStart, "Unterminated string.")
        } else {
            // The closing `"`.
            advance()
            // Trim the surrounding quotes.
            val value = source.substring(start + 1, current - 1)
            addToken(TokenType.STRING, value)
        }
    }

    private fun number() {
        while (peek().isDigit()) advance()

        // Look for a fractional part.
        if (peek() == '.' && peekNext().isDigit()) {
            // Consume the `.`
            advance()
            while (peek().isDigit()) advance()
        }

        addToken(TokenType.NUMBER, source.substring(start, current).toDoubleOrNull())
    }

    private fun match(expected: Char): Boolean =
        when {
            isAtEnd() -> false
            source[current] != expected -> false
            else -> {
                current++
                true
            }
        }

    private fun peek(): Char = if (isAtEnd()) '\u0000' else source[current]
    private fun peekNext(): Char = if (current + 1 >= source.length) '\u0000' else source[current + 1]
    private fun Char.isDigit() = this in '0'..'9'
    private fun Char.isAlpha() = this in 'a'..'z' || this in 'A'..'Z' || this == '_'
    private fun Char.isAlphaNumeric() = this.isAlpha() || this.isDigit()
    private fun isAtEnd(): Boolean = current >= source.length
    private fun advance(): Char = source[current++]
    private fun newLine() {
        line++
        lineStart = current
    }

    private fun addToken(type: TokenType, literal: Any? = null) {
        val text = source.substring(start, current)
        tokens += Token(type, text, literal, line, start - lineStart)
    }

    companion object {
        private val keywords: Map<String, TokenType> = mapOf(
            "and" to TokenType.AND,
            "class" to TokenType.CLASS,
            "else" to TokenType.ELSE,
            "false" to TokenType.FALSE,
            "for" to TokenType.FOR,
            "fun" to TokenType.FUN,
            "if" to TokenType.IF,
            "nil" to TokenType.NIL,
            "or" to TokenType.OR,
            "print" to TokenType.PRINT,
            "return" to TokenType.RETURN,
            "super" to TokenType.SUPER,
            "this" to TokenType.THIS,
            "true" to TokenType.TRUE,
            "var" to TokenType.VAR,
            "while" to TokenType.WHILE,
        )
    }
}
