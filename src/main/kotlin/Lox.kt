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
            hadError = false;
        }
    }

    private fun run(source: String) {
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        for (token in tokens) {
            println(token)
        }
    }

    fun error(line: Int, message: String) {
        report(line, "", message)
    }

    private fun report(int: Int, where: String, message: String) {
        System.err.println("[line $int] Error$where: $message")
        hadError = true
    }
}
