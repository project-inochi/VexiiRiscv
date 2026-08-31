package vexiiriscv.tester

import org.scalatest.funsuite.AnyFunSuite

class HartRegisterOptionsTester extends AnyFunSuite {
  private def parse(args: Seq[String]): (TestOptions, Boolean) = {
    val testOptions = new TestOptions()
    val parser = new scopt.OptionParser[Unit]("VexiiRiscv") {
      testOptions.addOptions(this)
    }
    testOptions -> parser.parse(args, ()).nonEmpty
  }

  test("hart register options accept repeated assignments and 64-bit patterns") {
    val (options, parsed) = parse(Seq(
      "--hart-register", "0=x10=0,x11=0xffffffffffffffff",
      "--hart-register", "0=x10=-2",
      "--hart-register", "1=x30=9223372036854775808,x31=0X8000000000000000"
    ))

    assert(parsed)
    assert(options.hartRegisterValues((0, 10)) == BigInt(-2))
    assert(options.hartRegisterValues((0, 11)) == BigInt("ffffffffffffffff", 16))
    assert(options.hartRegisterValues((1, 30)) == BigInt("9223372036854775808"))
    assert(options.hartRegisterValues((1, 31)) == BigInt("8000000000000000", 16))
  }

  test("hart register options reject malformed assignments") {
    val invalid = Seq(
      "0=x10",
      "-1=x10=0",
      "0=a0=0",
      "0=x32=0",
      "0=x1=0x10000000000000000",
      "0=x1=18446744073709551616"
    )

    invalid.foreach { value =>
      assert(!parse(Seq("--hart-register", value))._2, value)
    }
  }
}
