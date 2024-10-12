package cz.vojtasii.lox

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class AstPrinterTest : FunSpec({
    test("print simple expression") {
        val expression = Binary(
            Unary(
                Token(TokenType.MINUS, "-", null, 1, 0),
                Literal(LoxNumber(123.0)),
            ),
            Token(TokenType.STAR, "*", null, 1, 0),
            Grouping(
                Literal(LoxNumber(45.67)),
            ),
        )
        val repr = AstPrinter.visit(expression)
        repr shouldBe "(* (- 123.0) (group 45.67))"
    }
})
