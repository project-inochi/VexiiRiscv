package vexiiriscv.regfile

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import vexiiriscv.ParamSimple
import vexiiriscv.riscv.IntRegFile
import vexiiriscv.tester.TestBench

import scala.language.reflectiveCalls

class RegFileSimSetterTester extends AnyFunSuite {
  private def check(xlen: Int, regBased: Boolean): Unit = {
    val param = new ParamSimple()
    param.xlen = xlen
    if (regBased) {
      param.regFileSync = false
      param.regFileDualPortRam = false
      param.regFileRegBasedRam = true
    }

    SimConfig.compile(TestBench.makeDut(param, 1)).doSim { dut =>
      val cd = dut.clockDomain.withSyncReset()
      cd.forkStimulus(10)
      cd.waitSampling(64)

      val rf = dut.cores.head.host.find[RegfileService](_.rfSpec == IntRegFile).asInstanceOf[RegFilePlugin]
      val mask = (BigInt(1) << xlen) - 1

      def read(id: Int): BigInt = {
        if (rf.logic.regfile.fpga.asMem != null) {
          rf.logic.regfile.fpga.asMem.ram.getBigInt(id)
        } else {
          rf.logic.regfile.fpga.asReg.ram(id).toBigInt
        }
      }

      val values = if (xlen == 32) {
        Seq(0x80000000L, -1L, 0x12345678L)
      } else {
        Seq(Long.MinValue, -1L, 0x123456789abcdef0L)
      }

      for (((value, id), expected) <- values.zip(31 to 29 by -1).zip(values.map(BigInt(_) & mask))) {
        rf.simSetRegister(id, value)
        sleep(1)
        assert(read(id) == expected)
      }

      assert(read(0) == 0)
      rf.simSetRegister(0, -1L)
      sleep(1)
      assert(read(0) == 0)

      assertThrows[IllegalArgumentException](rf.simSetRegister(-1, 0L))
      assertThrows[IllegalArgumentException](rf.simSetRegister(rf.getPhysicalDepth, 0L))
    }
  }

  test("simSetRegister supports inferred Mem register files") {
    check(xlen = 32, regBased = false)
    check(xlen = 64, regBased = false)
  }

  test("simSetRegister supports Vec(Reg) register files") {
    check(xlen = 32, regBased = true)
    check(xlen = 64, regBased = true)
  }
}
