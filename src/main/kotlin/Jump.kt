package cz.vojtasii.lox

abstract class Jump : RuntimeException(null, null, false, false)

class ReturnJump(val value: LoxValue) : Jump()

class BreakJump : Jump()
