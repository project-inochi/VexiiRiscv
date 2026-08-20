package vexiiriscv.tester

import vexiiriscv.execute.lsu.LsuL1Plugin
import vexiiriscv.fetch.FetchL1Plugin
import vexiiriscv.memory.{MmuPlugin, MmuSpec}
import vexiiriscv.misc.{ImsicPlugin, PrivilegedPlugin}
import vexiiriscv.riscv.RiscvPlugin
import vexiiriscv.test.ImsicPeripheralEmulator
import vexiiriscv.{ExtensionManager, VexiiRiscv}
import spinal.lib.misc.aia.ImsicTrigger

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import scala.io.Source

/** Generates the device tree used by the Buildroot regression test.
  *
  * This deliberately lives in the tester sources: it describes the fixed
  * regression testbench platform, not a reusable VexiiRiscv SoC platform.
  */
private[tester] object BuildrootDeviceTree {
  final case class Artifacts(dts: File, dtb: File)

  private val InitrdStart = 0x81000000L
  private val Max32BitAddress = 0xffffffffL
  private val FirmwareLoadAddress = 0x80000000L
  private val ExternalFdtAddress = 0x80f80000L
  private val FdtMagic = 0xd00dfeedL

  def generate(dut: VexiiRiscv, rootfs: File, outputDirectory: File): Artifacts =
    generate(dut, rootfs, outputDirectory, "dtc")

  /** Package-visible overload used to exercise the hard-failure path without
    * changing PATH or adding a user-facing dtc configuration mechanism.
    */
  private[tester] def generate(
      dut: VexiiRiscv,
      rootfs: File,
      outputDirectory: File,
      dtcExecutable: String
  ): Artifacts = {
    require(rootfs.isFile, s"Buildroot rootfs does not exist or is not a file: ${rootfs.getAbsolutePath}")

    val riscv = dut.host.get[RiscvPlugin].getOrElse {
      throw new IllegalArgumentException("Buildroot device tree requires a RiscvPlugin")
    }
    require(riscv.hartCount == 1,
      s"Buildroot regression supports exactly one hart, but the DUT has ${riscv.hartCount}")

    val privileged = dut.host.get[PrivilegedPlugin].getOrElse {
      throw new IllegalArgumentException("Buildroot device tree requires a PrivilegedPlugin")
    }
    require(privileged.hartIds != null && privileged.hartIds == Seq(0),
      s"Buildroot regression requires fixed hart ID 0, but the DUT has ${Option(privileged.hartIds).getOrElse(Seq.empty).mkString(",")}")

    val isa = riscv.isa.map(_.toLowerCase)
    val isaExtensions = ExtensionManager
      .getIsaNameArray(isa)
      .filterNot(Set("s", "u", "g"))

    val mmu = dut.host.get[MmuPlugin].getOrElse {
      throw new IllegalArgumentException("Buildroot device tree requires an MmuPlugin")
    }
    val mmuType = mmu.spec match {
      case MmuSpec.sv32 =>
        require(riscv.xlen == 32, s"Sv32 cannot describe an RV${riscv.xlen} DUT")
        "riscv,sv32"
      case MmuSpec.sv39 =>
        require(riscv.xlen == 64, s"Sv39 cannot describe an RV${riscv.xlen} DUT")
        "riscv,sv39"
      case unsupported =>
        throw new IllegalArgumentException(s"Unsupported Buildroot MMU specification: $unsupported")
    }

    val fetchL1 = dut.host.get[FetchL1Plugin]
    val lsuL1 = dut.host.get[LsuL1Plugin]
    if (isa.contains("zicbom") && lsuL1.isEmpty) {
      throw new IllegalArgumentException(
        "The DUT reports Zicbom but has no LSU L1 cache from which to derive riscv,cbom-block-size"
      )
    }

    val imsicNodes = buildImsicNodes(dut, privileged)

    val initrdEnd = Math.addExact(InitrdStart, Files.size(rootfs.toPath))
    require(initrdEnd <= Max32BitAddress,
      f"Buildroot initrd end 0x$initrdEnd%x cannot be represented by this platform's one-cell address")

    Files.createDirectories(outputDirectory.toPath)
    val artifacts = Artifacts(
      new File(outputDirectory, "linux.dts"),
      new File(outputDirectory, "linux.dtb")
    )

    val extensionValues = isaExtensions.map(ext => "\"" + ext + "\"").mkString(", ")
    val fetchCacheProperties = fetchL1.map(p => cacheProperties("i", p.lineSize, p.setCount, p.wayCount)).getOrElse("")
    val lsuCacheProperties = lsuL1.map(p => cacheProperties("d", p.lineSize, p.setCount, p.wayCount)).getOrElse("")
    val cbomProperty = if (isa.contains("zicbom")) {
      val lineSize = lsuL1.get.lineSize
      s"\t\t\triscv,cbom-block-size = <$lineSize>;\n"
    } else ""

    val dts =
      s"""/dts-v1/;
         |
         |/ {
         |\tcompatible = "spinal,vexiiriscv";
         |\t#address-cells = <1>;
         |\t#size-cells = <1>;
         |
         |\tchosen {
         |\t\tbootargs = "rootwait console=hvc0 earlycon=sbi root=/dev/ram0 init=/sbin/init";
         |\t\tlinux,initrd-start = <0x${InitrdStart.toHexString}>;
         |\t\tlinux,initrd-end = <0x${initrdEnd.toHexString}>;
         |\t};
         |
         |\tcpus {
         |\t\t#address-cells = <1>;
         |\t\t#size-cells = <0>;
         |\t\ttimebase-frequency = <100000000>;
         |
         |\t\tcpu@0 {
         |\t\t\tcompatible = "spinal,vexiiriscv", "riscv";
         |\t\t\treg = <0>;
         |\t\t\tdevice_type = "cpu";
         |\t\t\tmmu-type = "$mmuType";
         |\t\t\triscv,isa-base = "rv${riscv.xlen}i";
         |\t\t\triscv,isa-extensions = $extensionValues;
         |$fetchCacheProperties$lsuCacheProperties$cbomProperty
         |\t\t\tcpu0_intc: interrupt-controller {
         |\t\t\t\tcompatible = "riscv,cpu-intc";
         |\t\t\t\tinterrupt-controller;
         |\t\t\t\t#interrupt-cells = <1>;
         |\t\t\t};
         |\t\t};
         |\t};
         |
         |\tmemory@80000000 {
         |\t\tdevice_type = "memory";
         |\t\treg = <0x80000000 0x40000000>;
         |\t};
         |
         |\tsoc {
         |\t\tcompatible = "simple-bus";
         |\t\t#address-cells = <1>;
         |\t\t#size-cells = <1>;
         |\t\tranges;
         |
         |\t\tserial0: serial@10000000 {
         |\t\t\tcompatible = "spinal,vexiiriscv-uart";
         |\t\t\treg = <0x10000000 0x1000>;
         |\t\t};
         |
         |\t\tclint: timer@10010000 {
         |\t\t\tcompatible = "sifive,clint0";
         |\t\t\treg = <0x10010000 0x10000>;
         |\t\t\tinterrupts-extended = <&cpu0_intc 3>, <&cpu0_intc 7>;
         |\t\t};
         |$imsicNodes\t};
         |
         |\tpoweroff {
         |\t\tcompatible = "spinal-poweroff";
         |\t};
         |};
         |""".stripMargin

    Files.write(
      artifacts.dts.toPath,
      dts.getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE
    )
    compile(dtcExecutable, artifacts)
    artifacts
  }

  private def buildImsicNodes(dut: VexiiRiscv, privileged: PrivilegedPlugin): String = {
    val privilegedWithImsic = privileged.p.withImsic
    val imsicPlugin = dut.host.get[ImsicPlugin]
    require(privilegedWithImsic == imsicPlugin.nonEmpty,
      s"Inconsistent IMSIC configuration: PrivilegedPlugin withImsic=$privilegedWithImsic, " +
        s"ImsicPlugin present=${imsicPlugin.nonEmpty}")

    if (!privilegedWithImsic) return ""

    val numIds = privileged.p.imsicInterrupts - 1

    val pageSize = ImsicTrigger.interruptFileSize
    val machineLayout = ImsicPeripheralEmulator.machineLayout
    val machineHartSize = machineLayout.mapping.interruptFileHartSize

    val machineNode =
      s"""
         |\t\timsic_m: interrupt-controller@${machineLayout.base.toHexString} {
         |\t\t\tcompatible = "riscv,imsics";
         |\t\t\treg = <0x${machineLayout.base.toHexString} 0x${machineHartSize.toString(16)}>;
         |\t\t\tinterrupt-controller;
         |\t\t\t#interrupt-cells = <0>;
         |\t\t\tmsi-controller;
         |\t\t\t#msi-cells = <0>;
         |\t\t\tinterrupts-extended = <&cpu0_intc 11>;
         |\t\t\triscv,num-ids = <$numIds>;
         |\t\t};
         |""".stripMargin

    if (!privileged.p.withSupervisor) return machineNode

    val supervisorLayout = ImsicPeripheralEmulator.supervisorLayout
    val supervisorHartSize = supervisorLayout.mapping.interruptFileHartSize
    require(supervisorHartSize % pageSize == 0,
      s"Buildroot supervisor IMSIC hart window $supervisorHartSize is not an integer multiple of the $pageSize-byte page size")
    val supervisorPages = supervisorHartSize / pageSize
    require(isPowerOfTwo(supervisorPages),
      s"Buildroot supervisor IMSIC hart window must contain a power-of-two number of pages, but contains $supervisorPages")
    val guestIndexBits = supervisorPages.bitLength - 1

    val supervisorNode =
      s"""
         |\t\timsic_s: interrupt-controller@${supervisorLayout.base.toHexString} {
         |\t\t\tcompatible = "riscv,imsics";
         |\t\t\treg = <0x${supervisorLayout.base.toHexString} 0x${supervisorHartSize.toString(16)}>;
         |\t\t\tinterrupt-controller;
         |\t\t\t#interrupt-cells = <0>;
         |\t\t\tmsi-controller;
         |\t\t\t#msi-cells = <0>;
         |\t\t\tinterrupts-extended = <&cpu0_intc 9>;
         |\t\t\triscv,num-ids = <$numIds>;
         |\t\t\triscv,guest-index-bits = <$guestIndexBits>;
         |\t\t};
         |""".stripMargin

    machineNode + supervisorNode
  }

  private def isPowerOfTwo(value: BigInt): Boolean =
    value > 0 && (value & (value - 1)) == 0

  /** Makes the prebuilt fw_jump firmware consume the externally loaded DTB.
    *
    * The Buildroot OpenSBI binaries embed their build-time DTB and normally
    * copy it over FW_JUMP_FDT_ADDR during startup.  Regression loads a dynamic
    * DTB at that address instead, so patch the single AUIPC/ADDI pair which
    * supplies fw_platform_init with fw_fdt_bin.  The firmware code and ABI are
    * otherwise unchanged, and the prebuilt image itself is never modified.
    */
  private[tester] def prepareOpenSbiFirmware(
      sourceFirmware: File,
      outputDirectory: File
  ): File = {
    require(sourceFirmware.isFile,
      s"Buildroot OpenSBI firmware does not exist or is not a file: ${sourceFirmware.getAbsolutePath}")

    val bytes = Files.readAllBytes(sourceFirmware.toPath)
    val embeddedFdts = (0 to bytes.length - 8).filter { offset =>
      readBigEndian32(bytes, offset) == FdtMagic && {
        val size = readBigEndian32(bytes, offset + 4)
        size >= 40 && size <= bytes.length.toLong - offset
      }
    }
    require(embeddedFdts.size == 1,
      s"Expected exactly one embedded FDT in ${sourceFirmware.getAbsolutePath}, found ${embeddedFdts.size}")

    val embeddedFdtAddress = Math.addExact(FirmwareLoadAddress, embeddedFdts.head.toLong)
    val references = (0 to bytes.length - 8 by 2).filter { offset =>
      val auipc = readLittleEndian32(bytes, offset)
      val addi = readLittleEndian32(bytes, offset + 4)
      isAuipcA1(auipc) && isAddiA1A1(addi) &&
        decodePcRelativeAddress(offset, auipc, addi) == embeddedFdtAddress
    }
    require(references.size == 1,
      s"Expected exactly one OpenSBI fw_fdt_bin reference in ${sourceFirmware.getAbsolutePath}, found ${references.size}")

    val referenceOffset = references.head
    val (auipc, addi) = encodePcRelativeAddress(referenceOffset, ExternalFdtAddress)
    writeLittleEndian32(bytes, referenceOffset, auipc)
    writeLittleEndian32(bytes, referenceOffset + 4, addi)

    Files.createDirectories(outputDirectory.toPath)
    val output = new File(outputDirectory, "fw_jump_external_fdt.bin")
    Files.write(
      output.toPath,
      bytes,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE
    )
    output
  }

  private def cacheProperties(kind: String, lineSize: Int, setCount: Int, wayCount: Int): String = {
    val size = Math.multiplyExact(Math.multiplyExact(lineSize.toLong, setCount.toLong), wayCount.toLong)
    s"\t\t\t$kind-cache-block-size = <$lineSize>;\n" +
      s"\t\t\t$kind-cache-sets = <$setCount>;\n" +
      s"\t\t\t$kind-cache-size = <$size>;\n"
  }

  private def compile(dtcExecutable: String, artifacts: Artifacts): Unit = {
    Files.deleteIfExists(artifacts.dtb.toPath)
    val command = Seq(
      dtcExecutable,
      "-I", "dts",
      "-O", "dtb",
      "-o", artifacts.dtb.getAbsolutePath,
      artifacts.dts.getAbsolutePath
    )
    val builder = new ProcessBuilder(command: _*).redirectErrorStream(true)
    val process = try {
      builder.start()
    } catch {
      case e: IOException =>
        throw new IllegalStateException(
          s"Failed to start device-tree compiler for ${artifacts.dtb.getAbsolutePath}: ${command.mkString(" ")}",
          e
        )
    }

    val source = Source.fromInputStream(process.getInputStream, StandardCharsets.UTF_8.name())
    val output = try source.mkString finally source.close()
    val exitCode = process.waitFor()
    if (exitCode != 0) {
      throw new IllegalStateException(
        s"Device-tree compiler failed with exit code $exitCode for ${artifacts.dtb.getAbsolutePath}\n" +
          s"Command: ${command.mkString(" ")}\n$output"
      )
    }
    if (!artifacts.dtb.isFile) {
      throw new IllegalStateException(
        s"Device-tree compiler returned success but did not create ${artifacts.dtb.getAbsolutePath}\n" +
          s"Command: ${command.mkString(" ")}\n$output"
      )
    }
  }

  private def readBigEndian32(bytes: Array[Byte], offset: Int): Long =
    ((bytes(offset).toLong & 0xffL) << 24) |
      ((bytes(offset + 1).toLong & 0xffL) << 16) |
      ((bytes(offset + 2).toLong & 0xffL) << 8) |
      (bytes(offset + 3).toLong & 0xffL)

  private def readLittleEndian32(bytes: Array[Byte], offset: Int): Long =
    (bytes(offset).toLong & 0xffL) |
      ((bytes(offset + 1).toLong & 0xffL) << 8) |
      ((bytes(offset + 2).toLong & 0xffL) << 16) |
      ((bytes(offset + 3).toLong & 0xffL) << 24)

  private def writeLittleEndian32(bytes: Array[Byte], offset: Int, value: Long): Unit = {
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
    bytes(offset + 2) = (value >>> 16).toByte
    bytes(offset + 3) = (value >>> 24).toByte
  }

  private def isAuipcA1(instruction: Long): Boolean =
    (instruction & 0xfffL) == 0x597L

  private def isAddiA1A1(instruction: Long): Boolean =
    (instruction & 0xfffffL) == 0x58593L

  private def decodePcRelativeAddress(offset: Int, auipc: Long, addi: Long): Long = {
    val pc = Math.addExact(FirmwareLoadAddress, offset.toLong)
    val upper = signExtend(auipc & 0xfffff000L, 32)
    val lower = signExtend((addi >>> 20) & 0xfffL, 12)
    Math.addExact(Math.addExact(pc, upper), lower)
  }

  private def encodePcRelativeAddress(offset: Int, target: Long): (Long, Long) = {
    val pc = Math.addExact(FirmwareLoadAddress, offset.toLong)
    val delta = Math.subtractExact(target, pc)
    val upper = Math.floorDiv(Math.addExact(delta, 0x800L), 0x1000L)
    require(upper >= -(1L << 19) && upper < (1L << 19),
      f"External FDT address 0x$target%x is out of AUIPC range from firmware offset 0x$offset%x")
    val lower = delta - upper * 0x1000L
    require(lower >= -2048L && lower <= 2047L,
      s"Internal error encoding OpenSBI external FDT address: low immediate is $lower")

    val auipc = ((upper & 0xfffffL) << 12) | 0x597L
    val addi = ((lower & 0xfffL) << 20) | 0x58593L
    (auipc, addi)
  }

  private def signExtend(value: Long, width: Int): Long = {
    val shift = 64 - width
    (value << shift) >> shift
  }
}
