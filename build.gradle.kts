plugins {
    kotlin("jvm") version "2.0.20"
    id("org.jmailen.kotlinter") version "4.4.1"
}

group = "cz.vojtasii"
version = "1.0-SNAPSHOT"

val kotestVersion by properties

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    testImplementation("io.kotest:kotest-runner-junit5:$kotestVersion")
    testImplementation("io.kotest:kotest-assertions-core:$kotestVersion")
    testImplementation("io.kotest:kotest-framework-datatest:$kotestVersion")
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}