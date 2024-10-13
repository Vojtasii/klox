package cz.vojtasii.lox

import java.io.File
import kotlin.system.exitProcess


object Lox {
    private var hadError: Boolean = false
    private var hadRuntimeError: Boolean = false
    private val interpreter = Interpreter()

    @JvmStatic
    fun main(args: Array<String>) {
        when {
            args.size > 1 -> {
                println("Usage: klox [script]")
                exitProcess(64)
            }
            args.size == 1 -> runFile(args[0])
            else -> runPrompt()
        }
    }

    private fun runFile(file: String) {
        val source = File(file).readText()
        run(source, Parser.Mode.STANDARD)
        // Indicate an error in the exit code.
        if (hadError) exitProcess(65)
        if (hadRuntimeError) exitProcess(70)
    }

    private fun runPrompt() {
        while (true) {
            print("> ")
            val line = readlnOrNull() ?: break
            run(line, Parser.Mode.REPL)
            hadError = false
        }
    }

    private fun run(source: String, mode: Parser.Mode) {
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        val parser = Parser(tokens, mode)
        val statements = parser.parse()

        if (hadError) return

        interpreter.interpret(statements)
    }

    fun error(line: Int, column: Int, message: String) {
        report(line, column, "", message)
    }

    fun error(token: Token, message: String) {
        if (token.type == TokenType.EOF) {
            report(token.line, token.column, " at end", message)
        } else {
            report(token.line, token.column, " at '" + token.lexeme + "'", message)
        }
    }

    fun runtimeError(error: RuntimeError) {
        with(error.token) {
            System.err.println("${error.message}\n[$line:$column]")
        }
        hadRuntimeError = true
    }

    private fun report(line: Int, column: Int, where: String, message: String) {
        System.err.println("[$line:$column] Error$where: $message")
        hadError = true
    }
}
