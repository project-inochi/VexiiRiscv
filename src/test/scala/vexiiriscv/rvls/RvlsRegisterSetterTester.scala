package vexiiriscv.rvls

import org.scalatest.funsuite.AnyFunSuite
import rvls.spinal.{FileBackend, RvlsBackend, TraceBackend}

import java.io.File
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import scala.sys.process.Process

class RvlsRegisterSetterTester extends AnyFunSuite {
  private val base = 0x80000000L
  private val sltiX2X1Zero = 0x0000a113
  private val addiX2X0Zero = 0x00000113
  private val program = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
    .putInt(sltiX2X1Zero)
    .putInt(addiX2X0Zero)
    .array()

  private def exercise(backend: TraceBackend, xlen: Int, value: Long): Unit = {
    backend.newCpuMemoryView(0, 1, 1)
    backend.newCpu(0, s"RV${xlen}I", "M", xlen, 0, 0, 0, 0)
    backend.addRegion(0, 0, base, 0x1000)
    backend.loadBytes(base, program)
    backend.setPc(0, base)

    backend.setRegister(0, 1, value)
    backend.writeRf(0, 0, 2, 1)
    backend.commit(0, base, sltiX2X1Zero)

    backend.setRegister(0, 0, -1L)
    backend.writeRf(0, 0, 2, 0)
    backend.commit(0, base + 4, addiX2X0Zero)
  }

  test("setRegister updates RV32 and RV64 integer state through JNI") {
    assume(new File("ext/rvls/build/apps/rvls.so").isFile)

    for ((xlen, value) <- Seq(32 -> 0x80000000L, 64 -> Long.MinValue)) {
      val backend = new RvlsBackend(Files.createTempDirectory(s"rvls-register-rv$xlen").toFile)
      try {
        exercise(backend, xlen, value)
        assertThrows[IllegalArgumentException](backend.setRegister(0, -1, 0))
        assertThrows[IllegalArgumentException](backend.setRegister(0, 32, 0))
      } finally {
        backend.close()
      }
    }
  }

  test("FileBackend setRegister commands replay through the ASCII frontend") {
    val executable = new File("ext/rvls/build/apps/rvls").getAbsoluteFile
    val library = new File("ext/rvls/build/apps/rvls.so").getAbsoluteFile
    assume(executable.isFile && library.isFile)

    for ((xlen, value) <- Seq(32 -> 0x80000000L, 64 -> Long.MinValue)) {
      val workspace = Files.createTempDirectory(s"rvls-register-ascii-rv$xlen")
      val trace = workspace.resolve("trace.log").toFile
      val backend = new FileBackend(trace)
      exercise(backend, xlen, value)
      backend.close()

      val libraryLink = workspace.resolve("build/apps/rvls.so")
      Files.createDirectories(libraryLink.getParent)
      Files.createSymbolicLink(libraryLink, library.toPath)

      assert(Process(Seq(executable.getAbsolutePath, "--file", trace.getAbsolutePath), workspace.toFile).! == 0)
    }
  }
}
