package cz.vojtasii.lox

import io.kotest.assertions.throwables.shouldNotThrowAnyUnit
import io.kotest.core.spec.style.FunSpec
import io.kotest.datatest.WithDataTestName
import io.kotest.datatest.withData
import java.nio.file.Path
import kotlin.io.path.name
import kotlin.io.path.toPath

internal class LoxTest :
    FunSpec({
        val data =
            LoxTest::class.java.classLoader
                .getResource("lox")!!
                .toURI()
                .toPath()
                .toFile()
                .walkTopDown()
                .filter { it.isFile && it.extension == "lox" }
                .map { LoxFileData(it.absolutePath) }

        context("Interpreting Lox code") {
            withData(data) {
                println("=== Run ${it.dataTestName()} ===")
                shouldNotThrowAnyUnit {
                    Lox.main(arrayOf(it.path))
                }
            }
        }
    })

private data class LoxFileData(
    val path: String,
) : WithDataTestName {
    override fun dataTestName(): String = Path.of(path).name
}
