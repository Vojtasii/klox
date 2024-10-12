package cz.vojtasii.lox

import java.io.File
import kotlin.system.exitProcess


object Lox {

    private var hadError: Boolean = false

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
        run(source)
        // Indicate an error in the exit code.
        if (hadError) exitProcess(65)
    }

    private fun runPrompt() {
        while (true) {
            print("> ")
            val line = readlnOrNull() ?: break
            run(line)
            hadError = false
        }
    }

    private fun run(source: String) {
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        val parser = Parser(tokens)
        val expression = parser.parse()

        if (hadError || expression == null) return

        println(AstPrinter.visit(expression))
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

    private fun report(line: Int, column: Int, where: String, message: String) {
        System.err.println("[$line:$column] Error$where: $message")
        hadError = true
    }
}
