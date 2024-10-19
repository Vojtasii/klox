package cz.vojtasii.lox

import io.kotest.matchers.Matcher
import io.kotest.matchers.MatcherResult

fun haveType(expected: TokenType) =
    Matcher<Token> { value ->
        MatcherResult(
            value.type == expected,
            { "Token type was ${value.type}, expected $expected" },
            { "Token type was $expected, expected it not to be" },
        )
    }
