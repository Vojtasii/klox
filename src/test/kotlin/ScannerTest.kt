package cz.vojtasii.lox

import io.kotest.core.spec.style.FunSpec
import io.kotest.datatest.WithDataTestName
import io.kotest.datatest.withData
import io.kotest.matchers.collections.shouldMatchEach
import io.kotest.matchers.should

internal class ScannerTest :
    FunSpec({
        context("Tokenizing valid Lox code") {
            withData(
                ScannerData(
                    "var a = 1;",
                    listOf(
                        TokenType.VAR,
                        TokenType.IDENTIFIER,
                        TokenType.EQUAL,
                        TokenType.NUMBER,
                        TokenType.SEMICOLON,
                        TokenType.EOF,
                    ),
                ),
                ScannerData(
                    """var b = "hello, world!";""",
                    listOf(
                        TokenType.VAR,
                        TokenType.IDENTIFIER,
                        TokenType.EQUAL,
                        TokenType.STRING,
                        TokenType.SEMICOLON,
                        TokenType.EOF,
                    ),
                ),
                ScannerData(
                    "1 - (2 * 3) < 4 == false",
                    listOf(
                        TokenType.NUMBER,
                        TokenType.MINUS,
                        TokenType.LEFT_PAREN,
                        TokenType.NUMBER,
                        TokenType.STAR,
                        TokenType.NUMBER,
                        TokenType.RIGHT_PAREN,
                        TokenType.LESS,
                        TokenType.NUMBER,
                        TokenType.EQUAL_EQUAL,
                        TokenType.FALSE,
                        TokenType.EOF,
                    ),
                ),
                ScannerData(
                    "fun add(a, b) /* block comment */ { return a + b; }",
                    listOf(
                        TokenType.FUN,
                        TokenType.IDENTIFIER,
                        TokenType.LEFT_PAREN,
                        TokenType.IDENTIFIER,
                        TokenType.COMMA,
                        TokenType.IDENTIFIER,
                        TokenType.RIGHT_PAREN,
                        TokenType.LEFT_BRACE,
                        TokenType.RETURN,
                        TokenType.IDENTIFIER,
                        TokenType.PLUS,
                        TokenType.IDENTIFIER,
                        TokenType.SEMICOLON,
                        TokenType.RIGHT_BRACE,
                        TokenType.EOF,
                    ),
                ),
            ) { (source, expected) ->
                val scanner = Scanner(source)
                val tokens = scanner.scanTokens()
                val assertions =
                    expected.map {
                        { t: Token -> t should haveType(it) }
                    }
                tokens.shouldMatchEach(assertions)
            }
        }
    })

private data class ScannerData(
    val source: String,
    val expected: List<TokenType>,
) : WithDataTestName {
    override fun dataTestName(): String = source
}
