package cz.vojtasii.lox

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class AstPrinterTest : FunSpec({
    test("print simple expression") {
        val expression = Binary(
            Unary(
                Token(TokenType.MINUS, "-", null, 1),
                Literal(123),
            ),
            Token(TokenType.STAR, "*", null, 1),
            Grouping(
                Literal(45.67),
            ),
        )
        val repr = AstPrinter.visit(expression)
        repr shouldBe "(* (- 123) (group 45.67))"
    }
})
