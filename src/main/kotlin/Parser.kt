package cz.vojtasii.lox

class Parser(
    private val tokens: List<Token>,
    private val mode: Mode = Mode.STANDARD,
) {
    private var current = 0
    private var loopDepth = 0

    /**
     * program → statement* EOF
     */
    fun parse(): List<Stmt> =
        buildList {
            while (!isAtEnd()) {
                val decl = declaration()
                if (decl != null) {
                    add(decl)
                }
            }
        }

    /**
     * declaration → "fun" function
     *             | "var" varDecl
     *             | statement
     */
    private fun declaration(): Stmt? = try {
        when {
            match(TokenType.FUN) -> function(Function.Kind.FUNCTION)
            match(TokenType.VAR) -> varDeclaration()
            else -> statement()
        }
    } catch (error: ParseError) {
        synchronize()
        null
    }

    /**
     * varDecl → IDENTIFIER ( "=" expression )? ";"
     */
    private fun varDeclaration(): Stmt {
        val name = consume(TokenType.IDENTIFIER, "Expect variable name.")

        val initializer = if (match(TokenType.EQUAL)) expression() else null

        consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")

        return Var(name, initializer)
    }

    /**
     * whileStmt → "while" "(" expression ")" statement
     */
    private fun whileStatement(): Stmt {
        loopDepth++
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
        val condition = expression()
        consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")
        val body = statement()

        return While(condition, body).also {
            loopDepth--
        }
    }

    /**
     * forStmt → "for" "(" ( "var" varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
     */
    private fun forStatement(): Stmt {
        loopDepth++
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")

        val initializer = when {
            match(TokenType.SEMICOLON) -> null
            match(TokenType.VAR) -> varDeclaration()
            else -> expressionStatement()
        }

        val condition = when {
            !check(TokenType.SEMICOLON) -> expression()
            else -> Literal(LoxBoolean(true))
        }
        consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")

        val increment = when {
            !check(TokenType.RIGHT_PAREN) -> expression()
            else -> null
        }
        consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")

        var body = statement()

        if (increment != null) {
            body = Block(listOf(body, Expression(increment)))
        }

        body = While(condition, body)

        if (initializer != null) {
            body = Block(listOf(initializer, body))
        }

        return body.also {
            loopDepth--
        }
    }

    /**
     * statement → exprStmt
     *           | forStmt
     *           | ifStmt
     *           | printStmt
     *           | whileStmt
     *           | returnStmt
     *           | breakStmt
     *           | block
     */
    private fun statement(): Stmt {
        return when {
            match(TokenType.FOR) -> forStatement()
            match(TokenType.IF) -> ifStatement()
            match(TokenType.PRINT) -> printStatement()
            match(TokenType.RETURN) -> returnStatement()
            match(TokenType.WHILE) -> whileStatement()
            match(TokenType.BREAK) -> breakStatement()
            match(TokenType.LEFT_BRACE) -> Block(block())
            else -> expressionStatement()
        }
    }

    /**
     * ifStmt → "if" "(" expression ")" statement ( "else" statement )?
     */
    private fun ifStatement(): Stmt {
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
        val condition = expression()
        consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")

        val thenBranch = statement()
        val elseBranch = if (match(TokenType.ELSE)) statement() else null

        return If(condition, thenBranch, elseBranch)
    }

    /**
     * printStmt → "print" expression ";"
     */
    private fun printStatement(): Stmt {
        val expr = expression()
        consume(TokenType.SEMICOLON, "Expect ';' after value.")
        return Print(expr)
    }

    /**
     * returnStmt → "return" expression? ";"
     */
    private fun returnStatement(): Stmt {
        val keyword = previous()
        val value = if (check(TokenType.SEMICOLON)) null else expression()

        consume(TokenType.SEMICOLON, "Expect ';' after return value.")
        return Return(keyword, value)
    }

    /**
     * exprStmt → expression ";"
     */
    private fun expressionStatement(): Stmt {
        val expr = expression()
        return when (mode) {
            Mode.STANDARD -> {
                consume(TokenType.SEMICOLON, "Expect ';' after value.")
                Expression(expr)
            }
            Mode.REPL -> when {
                match(TokenType.SEMICOLON) -> Expression(expr)
                isAtEnd() -> Print(expr)
                else -> throw error(peek(), "Invalid expression or missing ';' after value.")
            }
        }
    }

    /**
     * function   → IDENTIFIER "(" parameters? ")" block
     * parameters → IDENTIFIER ( "," IDENTIFIER )*
     */
    private fun function(kind: Function.Kind): Function {
        val name = consume(TokenType.IDENTIFIER, "Expect $kind name.")
        consume(TokenType.LEFT_PAREN, "Expect '(' after $kind name.")
        val parameters = if (check(TokenType.RIGHT_PAREN)) {
            emptyList()
        } else buildList {
            do {
                if (size >= Lox.MAX_ARGUMENTS) {
                    error(peek(), "Can't have more than ${Lox.MAX_ARGUMENTS} parameters.")
                }

                add(consume(TokenType.IDENTIFIER, "Expect parameter name."))
            } while (match(TokenType.COMMA))
        }
        consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.")

        consume(TokenType.LEFT_BRACE, "Expect '{' before $kind body.")
        val body = block()
        return Function(name, parameters, body)
    }

    private fun breakStatement(): Stmt {
        val breakToken = previous()
        if (loopDepth <= 0) {
            error(breakToken, "Statement 'break' is allowed only inside a loop.")
        }
        consume(TokenType.SEMICOLON, "Expect ';' after 'break'.")
        return Break
    }

    /**
     * block → "{" declaration* "}"
     */
    private fun block(): List<Stmt> = buildList {
        while (!check(TokenType.RIGHT_BRACE) && !isAtEnd()) {
            val decl = declaration()
            if (decl != null) {
                add(decl)
            }
        }

        consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
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
     * conditional → assignment ( '?' assignment ':' conditional )
     */
    private fun conditional(): Expr {
        val expr = assignment()

        return if (match(TokenType.QUESTION)) {
            val truly = assignment()
            consume(TokenType.COLON, "Expect ':' after first expression in ternary conditional.")
            val falsy = conditional()
            TernaryConditional(expr, truly, falsy)
        } else {
            expr
        }
    }

    /**
     * assignment → IDENTIFIER "=" assignment
     *            | logic_or
     */
    private fun assignment(): Expr {
        val expr = or()

        if (match(TokenType.EQUAL)) {
            val equals = previous()
            val value = assignment()

            if (expr is Variable) {
                val name = expr.name
                return Assign(name, value)
            } else {
                error(equals, "Invalid assignment target.")
            }
        }

        return expr
    }

    /**
     * logic_or → logic_and ( "or" logic_and )*
     */
    private fun or(): Expr {
        var expr = and()

        while (match(TokenType.OR)) {
            val operator = previous()
            val right = equality()
            expr = Logical(expr, operator, right)
        }

        return expr
    }

    /**
     * logic_and → equality ( "and" equality )*
     */
    private fun and(): Expr {
        var expr = equality()

        while (match(TokenType.AND)) {
            val operator = previous()
            val right = equality()
            expr = Logical(expr, operator, right)
        }

        return expr
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
     *       | call
     */
    private fun unary(): Expr {
        return if (match(TokenType.BANG, TokenType.MINUS)) {
            val operator = previous()
            val right = unary()
            Unary(operator, right)
        } else {
            call()
        }
    }

    /**
     * call → primary ( "(" arguments? ")" )*
     */
    private fun call(): Expr {
        var expr = primary()

        while (true) {
            if (match(TokenType.LEFT_PAREN)) {
                expr = finishCall(expr)
            } else {
                break
            }
        }

        return expr
    }

    /**
     * Implements 'arguments' rule while matching for ')'.
     *
     * arguments → argument ( "," argument )*
     */
    private fun finishCall(callee: Expr): Expr {
        val arguments = if (check(TokenType.RIGHT_PAREN)) {
            emptyList()
        } else buildList {
            if (size >= Lox.MAX_ARGUMENTS) {
                error(peek(), "Can't have more than ${Lox.MAX_ARGUMENTS} arguments.")
            }
            do {
                add(argument())
            } while (match(TokenType.COMMA))
        }

        val paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")

        return Call(callee, paren, arguments)
    }

    /**
     * Expression that disallows ',' operator.
     *
     * argument → conditional
     */
    private fun argument(): Expr = conditional()

    /**
     * primary → NUMBER | STRING | "true" | "false" | "nil"
     *         | "(" expression ")"
     *         | IDENTIFIER
     */
    private fun primary(): Expr {
        return when {
            match(TokenType.NUMBER, TokenType.STRING) -> Literal(previous().literal ?: LoxNil)
            match(TokenType.TRUE) -> Literal(LoxBoolean(true))
            match(TokenType.FALSE) -> Literal(LoxBoolean(false))
            match(TokenType.NIL) -> Literal(LoxNil)
            match(TokenType.LEFT_PAREN) -> {
                val expr = expression()
                consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
                Grouping(expr)
            }
            match(TokenType.IDENTIFIER) -> Variable(previous())
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
                TokenType.RETURN,
                    -> return
                else -> Unit
            }

            advance()
        }
    }

    enum class Mode {
        // Only a list of statements.
        STANDARD,

        // Allow a list of statements, or a standalone expression interpreted as a print statement.
        REPL,
    }

    private class ParseError : RuntimeException()
}