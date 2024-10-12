package cz.vojtasii.lox

class Parser(
    private val tokens: List<Token>,
) {
    private var current = 0

    fun parse(): Expr? =
        try {
            expression()
        } catch (error: ParseError) {
            null
        }

    /**
     * expression → conditional ( "," conditional )*
     */
    private fun expression(): Expr {
        var expr = conditional()

        // Comma binary operator.
        // Evaluate both left and right expressions, but use only the right expression.
        while (match(TokenType.COMMA)) {
            val operator = previous()
            val right = conditional()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    /**
     * conditional → equality ( '?' equality ':' conditional )
     */
    private fun conditional(): Expr {
        val expr = equality()

        return if (match(TokenType.QUESTION)) {
            val truly = equality()
            consume(TokenType.COLON, "Expect ':' after first expression in ternary conditional.")
            val falsy = conditional()
            TernaryConditional(expr, truly, falsy)
        } else {
            expr
        }
    }

    /**
     * equality → comparison ( ( "!=" | "==" ) comparison )*
     */
    private fun equality(): Expr {
        var expr = comparison()

        while (match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
            val operator = previous()
            val right = comparison()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    /**
     * comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
     */
    private fun comparison(): Expr {
        var expr = term()

        while (match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
            val operator = previous()
            val right = term()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    /**
     * term → factor ( ( "-" | "+" ) factor )*
     */
    private fun term(): Expr {
        var expr = factor()

        while (match(TokenType.MINUS, TokenType.PLUS)) {
            val operator = previous()
            val right = factor()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    /**
     * factor → unary ( ( "/" | "*" ) unary )*
     */
    private fun factor(): Expr {
        var expr = unary()

        while (match(TokenType.SLASH, TokenType.STAR)) {
            val operator = previous()
            val right = unary()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    /**
     * unary → ( "!" | "-" ) unary
     *       | primary
     */
    private fun unary(): Expr {
        return if (match(TokenType.BANG, TokenType.MINUS)) {
            val operator = previous()
            val right = unary()
            Unary(operator, right)
        } else {
            primary()
        }
    }

    /**
     * primary → NUMBER | STRING | "true" | "false" | "nil"
     *         | "(" expression ")"
     */
    private fun primary(): Expr {
        return when {
            match(TokenType.NUMBER, TokenType.STRING) -> Literal(previous().literal)
            match(TokenType.TRUE) -> Literal(true)
            match(TokenType.FALSE) -> Literal(false)
            match(TokenType.NIL) -> Literal(null)
            match(TokenType.LEFT_PAREN) -> {
                val expr = expression()
                consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
                Grouping(expr)
            }
            else -> throw error(peek(), "Expect expression.")
        }
    }

    private fun match(vararg types: TokenType): Boolean {
        return types.any { type -> check(type) }.also { isMatched ->
            if (isMatched) {
                advance()
            }
        }
    }

    private fun consume(type: TokenType, message: String): Token {
        return if (check(type)) {
            advance()
        } else {
            throw error(peek(), message)
        }
    }

    private fun check(type: TokenType): Boolean {
        return if (isAtEnd()) {
            false
        } else {
            peek().type == type
        }
    }

    private fun advance(): Token {
        if (!isAtEnd()) current++
        return previous()
    }

    private fun isAtEnd(): Boolean = peek().type == TokenType.EOF
    private fun peek(): Token = tokens[current]
    private fun previous(): Token = tokens[current - 1]

    private fun error(token: Token, message: String): ParseError {
        Lox.error(token, message)
        return ParseError()
    }

    private fun synchronize() {
        advance()

        while (!isAtEnd()) {
            if (previous().type == TokenType.SEMICOLON) return

            when (peek().type) {
                TokenType.CLASS,
                TokenType.FUN,
                TokenType.VAR,
                TokenType.FOR,
                TokenType.IF,
                TokenType.WHILE,
                TokenType.PRINT,
                TokenType.RETURN -> return
                else -> Unit
            }

            advance()
        }
    }

    private class ParseError : RuntimeException()
}