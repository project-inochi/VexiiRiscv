package vexiiriscv.tester

import com.google.gson.GsonBuilder
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import spinal.core.{isPow2, log2Up}
import vexiiriscv.{ExtensionList, ExtensionVersion, ParamSimple}

import java.io.{File, PrintWriter}
import java.util.{ArrayList, LinkedHashMap}
import scala.collection.mutable.ArrayBuffer

case class ActConfigGenOptions(
  outputDir: Option[File] = None,
  name: Option[String] = None,
  crossName: String = "riscv64-elf-",
  refModelExe: String = "sail_riscv_sim",
  includePrivTests: Boolean = true,
  overwrite: Boolean = false
)

/**
 * Generate a riscv-arch-test ACT4 configuration for vexiiriscv.tester.TestBench.
 *
 * This deliberately uses ParamSimple instead of SocConfig. The generated
 * memory map and run command target the standalone TestBench, not the Litex SoC.
 */
object ActConfigGen extends App {
  type JMap = LinkedHashMap[String, Object]
  type JList = ArrayList[Object]

  val param = new ParamSimple()
  var genOptions = ActConfigGenOptions()

  assert(new scopt.OptionParser[Unit]("ActConfigGen") {
    help("help").text("prints this usage text")
    param.addOptions(this)
    opt[String]("act4-output-dir") action { (v, c) => genOptions = genOptions.copy(outputDir = Some(new File(v))) }
    opt[String]("act4-name") action { (v, c) => genOptions = genOptions.copy(name = Some(v)) }
    opt[String]("cross-name") action { (v, c) => genOptions = genOptions.copy(crossName = v) }
    opt[String]("ref-model-exe") action { (v, c) => genOptions = genOptions.copy(refModelExe = v) }
    opt[Unit]("include-priv-tests") action { (v, c) => genOptions = genOptions.copy(includePrivTests = true) }
    opt[Unit]("no-priv-tests") action { (v, c) => genOptions = genOptions.copy(includePrivTests = false) }
    opt[Unit]("overwrite") action { (v, c) => genOptions = genOptions.copy(overwrite = true) }
  }.parse(args, ()).nonEmpty)

  val files = generate(param, genOptions)
  println(s"Generated ACT4 config files in ${files.head.getParentFile.getAbsolutePath}")
  files.foreach(f => println(s" - ${f.getName}"))

  def generate(param: ParamSimple, options: ActConfigGenOptions): Seq[File] = {
    val model = ActModel.from(param, options)
    validate(model)
    if (model.has("h")) {
      Console.err.println("WARNING: H extension is enabled in ParamSimple, but ACT4 currently undefines H_SUPPORTED in riscv_arch_test.h; H/HS/VS are not emitted in the ACT4 config.")
    }

    val dir = options.outputDir.getOrElse(new File("build/act4", model.name))
    if (!dir.exists() && !dir.mkdirs()) throw new RuntimeException(s"Could not create ACT4 output directory: ${dir.getAbsolutePath}")

    val files = Seq(
      new File(dir, "test_config.yaml") -> renderYaml(testConfigData(model)),
      new File(dir, s"${model.name}.yaml") -> renderYaml(udbConfigData(model)),
      new File(dir, "sail.json") -> renderJson(sailConfigData(model)),
      new File(dir, "rvmodel_macros.h") -> renderRvmodelMacros(model),
      new File(dir, "link.ld") -> renderLinkerScript(model),
      new File(dir, "run_cmd.txt") -> renderRunCommand(model)
    )

    for ((file, text) <- files) writeFile(file, text, options.overwrite)
    files.map(_._1)
  }

  def writeFile(file: File, text: String, overwrite: Boolean): Unit = {
    if (file.exists() && !overwrite) throw new RuntimeException(s"Refusing to overwrite ${file.getAbsolutePath}; pass --overwrite")
    val pw = new PrintWriter(file)
    try pw.write(text) finally pw.close()
  }

  def validate(m: ActModel): Unit = {
    if (m.xlen != 32 && m.xlen != 64) throw new IllegalArgumentException(s"Unsupported ACT4 XLEN ${m.xlen}; expected 32 or 64")
    if (!isPow2(m.pmpGranularity)) throw new IllegalArgumentException(s"Unsupported --pmp-granularity=${m.pmpGranularity}; ACT4 generation requires a power of two")
    if (m.pmpSize < 0) throw new IllegalArgumentException(s"Unsupported --pmp-size=${m.pmpSize}; expected a non-negative PMP entry count")
    if (!isExecutableTestBenchAddress(m.resetVector)) {
      throw new IllegalArgumentException(f"Unsupported reset vector 0x${m.resetVector}%x for TestBench ACT4 config; expected an address in 0x80000000..0xffffffff or 0x1000..0x1fff")
    }
  }

  def isExecutableTestBenchAddress(address: Long): Boolean = {
    (address >= 0x80000000L && address < 0x100000000L) ||
      (address >= 0x1000L && address < 0x2000L)
  }

  case class ActExtension(name: String, version: Option[ExtensionVersion]) {
    def toYamlData: JMap = {
      val ret = jmap("name" -> name)
      version.foreach(v => ret.put("version", s"= ${v.major}.${v.minor}"))
      ret
    }
  }

  case class ActModel(
    param: ParamSimple,
    name: String,
    xlen: Int,
    resetVector: Long,
    physicalWidth: Int,
    asidWidth: Int,
    pmpSize: Int,
    pmpGranularity: Int,
    crossName: String,
    refModelExe: String,
    includePrivTests: Boolean,
    isaNames: Seq[String],
    udbExtensions: Seq[ActExtension]
  ) {
    def has(ext: String): Boolean = param.checkISA(ext)
    def hasAll(exts: String*): Boolean = exts.forall(has)
    def hasZaamo: Boolean = has("zaamo")
    def hasZalrsc: Boolean = has("zalrsc")
    def compilerExe: String = crossName + "gcc"
    def objdumpExe: String = crossName + "objdump"
    def pmpGrain: Int = log2Up(pmpGranularity)
    def sailPmpCount: Int = {
      if (pmpSize == 0) 0
      else if (pmpSize <= 16) 16
      else if (pmpSize <= 64) 64
      else throw new IllegalArgumentException(s"Sail ACT4 PMP count only supports 0, 16 or 64 buckets; got usable PMP count ${pmpSize}")
    }
    def hpmCount: Int = if (has("zihpm")) (param.additionalPerformanceCounters max 0) min 29 else 0
    def hpmMaskHex: String = {
      var mask = BigInt(0)
      for (i <- 0 until hpmCount) mask |= BigInt(1) << (i + 3)
      "0x" + mask.toString(16).toUpperCase
    }
    def mmuMode: Option[String] = if (param.withMmu) Some(if (xlen == 32) "sv32" else "sv39") else None
  }

  object ActModel {
    def from(param: ParamSimple, options: ActConfigGenOptions): ActModel = {
      val isaNames = param.extension.getIsaNameArray().map(_.toLowerCase)
      val name = options.name.getOrElse(defaultName(param))
      ActModel(
        param = param,
        name = name,
        xlen = param.xlen,
        resetVector = param.resetVector,
        physicalWidth = param.physicalWidth,
        asidWidth = param.asidWidth,
        pmpSize = param.pmpParam.pmpSize,
        pmpGranularity = param.pmpParam.granularity,
        crossName = options.crossName,
        refModelExe = options.refModelExe,
        includePrivTests = options.includePrivTests,
        isaNames = isaNames,
        udbExtensions = udbExtensionList(param, isaNames)
      )
    }

    def defaultName(param: ParamSimple): String = {
      s"rv${param.xlen}${param.extension.getIsaStr()}"
    }

    def udbExtensionList(param: ParamSimple, isaNames: Seq[String]): Seq[ActExtension] = {
      val names = ArrayBuffer[String]()

      names ++= isaNames.filterNot(e => e == "g" || e == "h")
      if (param.checkISA("u")) names += "u"
      if (param.checkISA("s")) names += "s"
      param.withMmu match {
        case true => names += (if (param.xlen == 32) "sv32" else "sv39")
        case false =>
      }
      names += "sm"

      names.distinct.map { ext =>
        ActExtension(actName(ext), ExtensionList.versionOf(ext))
      }.toSeq
    }
  }

  def actName(ext: String): String = {
    val lower = ext.toLowerCase
    lower.head.toUpper + lower.tail
  }

  def testConfigData(m: ActModel): JMap = jmap(
    "name" -> m.name,
    "compiler_exe" -> m.compilerExe,
    "objdump_exe" -> m.objdumpExe,
    "ref_model_exe" -> m.refModelExe,
    "udb_config" -> s"${m.name}.yaml",
    "linker_script" -> "link.ld",
    "dut_include_dir" -> ".",
    "include_priv_tests" -> m.includePrivTests
  )

  def udbConfigData(m: ActModel): JMap = {
    val params = jmap()

    if (m.hasZaamo) params.put("MISALIGNED_AMO", toJava(false))
    if (m.hasZalrsc) params.putAll(jmap(
      "LRSC_RESERVATION_STRATEGY" -> "reserve exactly enough to cover the access",
      "LRSC_FAIL_ON_VA_SYNONYM" -> false,
      "LRSC_MISALIGNED_BEHAVIOR" -> "always raise access fault",
      "LRSC_FAIL_ON_NON_EXACT_LRSC" -> false
    ))
    if (m.has("a")) params.putAll(jmap(
      "MUTABLE_MISA_A" -> false
    ))
    if (m.has("m")) params.put("MUTABLE_MISA_M", toJava(false))
    if (m.has("f")) params.putAll(jmap(
      "MUTABLE_MISA_F" -> false,
      "HW_MSTATUS_FS_DIRTY_UPDATE" -> "precise",
      "MSTATUS_FS_LEGAL_VALUES" -> Seq(0, 1, 2, 3)
    ))
    if (m.has("d")) params.put("MUTABLE_MISA_D", toJava(false))
    if (m.has("c")) params.put("MUTABLE_MISA_C", toJava(false))
    if (m.hasAll("zba", "zbb", "zbc", "zbs")) params.put("MUTABLE_MISA_B", toJava(false))
    if (m.has("zicbom")) params.putAll(jmap(
      "CACHE_BLOCK_SIZE" -> 64,
      "FORCE_UPGRADE_CBO_INVAL_TO_FLUSH" -> false
    ))
    if (m.has("zicntr")) params.put("TIME_CSR_IMPLEMENTED", toJava(true))
    if (m.has("u")) params.putAll(jmap(
      "MUTABLE_MISA_U" -> false,
      "U_MODE_ENDIANNESS" -> "little",
      "UXLEN" -> Seq(m.xlen),
      "TRAP_ON_ECALL_FROM_U" -> true
    ))
    if (m.has("s")) params.putAll(jmap(
      "MUTABLE_MISA_S" -> false,
      "ASID_WIDTH" -> m.asidWidth,
      "S_MODE_ENDIANNESS" -> "little",
      "SXLEN" -> Seq(m.xlen),
      "REPORT_VA_IN_MTVAL_ON_LOAD_PAGE_FAULT" -> true,
      "REPORT_VA_IN_MTVAL_ON_STORE_AMO_PAGE_FAULT" -> true,
      "REPORT_VA_IN_MTVAL_ON_INSTRUCTION_PAGE_FAULT" -> true,
      "REPORT_VA_IN_STVAL_ON_BREAKPOINT" -> true,
      "REPORT_VA_IN_STVAL_ON_LOAD_MISALIGNED" -> true,
      "REPORT_VA_IN_STVAL_ON_STORE_AMO_MISALIGNED" -> true,
      "REPORT_VA_IN_STVAL_ON_INSTRUCTION_MISALIGNED" -> true,
      "REPORT_VA_IN_STVAL_ON_LOAD_ACCESS_FAULT" -> true,
      "REPORT_VA_IN_STVAL_ON_STORE_AMO_ACCESS_FAULT" -> true,
      "REPORT_VA_IN_STVAL_ON_INSTRUCTION_ACCESS_FAULT" -> true,
      "REPORT_VA_IN_STVAL_ON_LOAD_PAGE_FAULT" -> true,
      "REPORT_VA_IN_STVAL_ON_STORE_AMO_PAGE_FAULT" -> true,
      "REPORT_VA_IN_STVAL_ON_INSTRUCTION_PAGE_FAULT" -> true,
      "REPORT_ENCODING_IN_STVAL_ON_ILLEGAL_INSTRUCTION" -> true,
      "STVAL_WIDTH" -> m.xlen,
      "SCOUNTENABLE_EN" -> Seq.fill(32)(true),
      "STVEC_MODE_DIRECT" -> true,
      "STVEC_MODE_VECTORED" -> true,
      "SATP_MODE_BARE" -> true,
      "TRAP_ON_ECALL_FROM_S" -> true,
      "MSTATUS_VS_LEGAL_VALUES" -> Seq(0, 1, 2, 3),
      "MSTATUS_TVM_IMPLEMENTED" -> true
    ))

    val countInhibit = (0 until 32).map(i => i != 1)
    val hpmEn = (0 until 32).map(i => i >= 3 && i < 3 + m.hpmCount)

    params.putAll(jmap(
      "MXLEN" -> m.xlen,
      "PRECISE_SYNCHRONOUS_EXCEPTIONS" -> true,
      "TRAP_ON_ECALL_FROM_M" -> true,
      "TRAP_ON_EBREAK" -> true,
      "MARCHID_IMPLEMENTED" -> true,
      "ARCH_ID_VALUE" -> "0x24",
      "MIMPID_IMPLEMENTED" -> true,
      "IMP_ID_VALUE" -> "0x100",
      "VENDOR_ID_BANK" -> "0xC",
      "VENDOR_ID_OFFSET" -> "0x2",
      "MISALIGNED_LDST" -> true,
      "MISALIGNED_LDST_EXCEPTION_PRIORITY" -> "low",
      "MISALIGNED_MAX_ATOMICITY_GRANULE_SIZE" -> 4096,
      "MISALIGNED_SPLIT_STRATEGY" -> "custom",
      "TRAP_ON_ILLEGAL_WLRL" -> false,
      "TRAP_ON_UNIMPLEMENTED_INSTRUCTION" -> true,
      "TRAP_ON_RESERVED_INSTRUCTION" -> true,
      "TRAP_ON_UNIMPLEMENTED_CSR" -> true,
      "REPORT_VA_IN_MTVAL_ON_BREAKPOINT" -> true,
      "REPORT_VA_IN_MTVAL_ON_LOAD_MISALIGNED" -> true,
      "REPORT_VA_IN_MTVAL_ON_STORE_AMO_MISALIGNED" -> true,
      "REPORT_VA_IN_MTVAL_ON_INSTRUCTION_MISALIGNED" -> true,
      "REPORT_VA_IN_MTVAL_ON_LOAD_ACCESS_FAULT" -> true,
      "REPORT_VA_IN_MTVAL_ON_STORE_AMO_ACCESS_FAULT" -> true,
      "REPORT_VA_IN_MTVAL_ON_INSTRUCTION_ACCESS_FAULT" -> true,
      "REPORT_ENCODING_IN_MTVAL_ON_ILLEGAL_INSTRUCTION" -> true,
      "MTVAL_WIDTH" -> m.xlen,
      "CONFIG_PTR_ADDRESS" -> 0,
      "PMA_GRANULARITY" -> 3,
      "PHYS_ADDR_WIDTH" -> m.physicalWidth,
      "M_MODE_ENDIANNESS" -> "little",
      "MISA_CSR_IMPLEMENTED" -> true,
      "MTVEC_ACCESS" -> "rw",
      "MTVEC_MODES" -> Seq(0, 1),
      "MTVEC_BASE_ALIGNMENT_DIRECT" -> 4,
      "MTVEC_BASE_ALIGNMENT_VECTORED" -> 4,
      "MTVEC_ILLEGAL_WRITE_BEHAVIOR" -> "retain",
      "NUM_PMP_ENTRIES" -> m.pmpSize,
      "PMP_GRANULARITY" -> m.pmpGrain,
      "HPM_COUNTER_EN" -> hpmEn,
      "HPM_EVENTS" -> Seq(0),
      "MCOUNTINHIBIT_IMPLEMENTED" -> true,
      "COUNTINHIBIT_EN" -> countInhibit,
      "MCOUNTENABLE_EN" -> Seq.fill(32)(true)
    ))

    jmap(
      "$schema" -> "config_schema.json#",
      "kind" -> "architecture configuration",
      "type" -> "fully configured",
      "name" -> m.name,
      "description" -> "Generated VexiiRiscv TestBench ACT4 configuration",
      "implemented_extensions" -> m.udbExtensions.map(_.toYamlData),
      "params" -> params
    )
  }

  def renderLinkerScript(m: ActModel): String = {
    s"""|OUTPUT_ARCH( "riscv" )
        |ENTRY(rvtest_entry_point)
        |
        |SECTIONS
        |{
        |  . = 0x${m.resetVector.toHexString};
        |  .text.init    : { *(.text.init) }
        |  .text.rvtest  : { *(.text.rvtest) *(.text.rvtest.*) }
        |  . = ALIGN(0x4000);
        |  .data         : { *(.data) }
        |  . = ALIGN(0x1000);
        |  .text.rvmodel : { *(.text.rvmodel) *(.text.rvmodel.*) *(.text) *(.text.*) }
        |  . = ALIGN(0x1000);
        |  _end = .;
        |}
        |""".stripMargin
  }

  def renderRvmodelMacros(m: ActModel): String = {
    s"""|#ifndef _RVMODEL_MACROS_H
        |#define _RVMODEL_MACROS_H
        |
        |#define RVMODEL_DATA_SECTION \\
        |        .pushsection .tohost,"aw",@progbits;                \\
        |        .align 8; .global tohost; tohost: .dword 0;         \\
        |        .align 8; .global fromhost; fromhost: .dword 0;     \\
        |        .popsection;
        |
        |#define RVMODEL_HALT_PASS  \\
        |  li x1, 1                ;\\
        |  la t0, tohost           ;\\
        |  write_tohost_pass:      ;\\
        |    sw x1, 0(t0)          ;\\
        |    sw x0, 4(t0)          ;\\
        |  pass:                   ;\\
        |    nop                   ;\\
        |    j pass                ;\\
        |
        |#define RVMODEL_HALT_FAIL \\
        |  li x1, 3                ;\\
        |  la t0, tohost           ;\\
        |  write_tohost_fail:      ;\\
        |    sw x1, 0(t0)          ;\\
        |    sw x0, 4(t0)          ;\\
        |  fail:                   ;\\
        |    nop                   ;\\
        |    j fail                ;\\
        |
        |.EQU UART_BASE_ADDR, 0x10000000
        |.EQU UART_PUTC, (UART_BASE_ADDR + 0x0)
        |
        |#define RVMODEL_IO_INIT(_R1, _R2, _R3) \\
        |  uart_init: ;
        |
        |#define RVMODEL_IO_WRITE_STR(_R1, _R2, _R3, _STR_PTR)               \\
        |1:                           ;                          \\
        |  lbu _R1, 0(_STR_PTR)       ;                          \\
        |  beqz _R1, 3f               ;                          \\
        |2:                           ;                          \\
        |  li _R2, UART_PUTC          ;                          \\
        |  sb _R1, 0(_R2)             ;                          \\
        |  addi _STR_PTR, _STR_PTR, 1 ;                          \\
        |  j 1b                       ;                          \\
        |3:
        |
        |#define RVMODEL_ACCESS_FAULT_ADDRESS 0x10000080
        |#define RVMODEL_INTERRUPT_LATENCY 10
        |
        |#define CLINT_BASE_ADDRESS 0x10010000
        |#define RVMODEL_TIMER_INT_SOON_DELAY 100
        |#define RVMODEL_MTIME_ADDRESS    (CLINT_BASE_ADDRESS + 0xBFF8)
        |#define RVMODEL_MTIMECMP_ADDRESS (CLINT_BASE_ADDRESS + 0x4000)
        |#define RVMODEL_MSIP_ADDRESS (CLINT_BASE_ADDRESS + 0x0)
        |
        |#define RVMODEL_SET_MEXT_INT(_R1, _R2)
        |#define RVMODEL_CLR_MEXT_INT(_R1, _R2)
        |
        |#define RVMODEL_SET_MSW_INT(_R1, _R2) \\
        |  li _R1, 1; \\
        |  li _R2, RVMODEL_MSIP_ADDRESS; \\
        |  sw _R1, 0(_R2);
        |
        |#define RVMODEL_CLR_MSW_INT(_R1, _R2) \\
        |  li _R2, RVMODEL_MSIP_ADDRESS; \\
        |  sw zero, 0(_R2);
        |
        |#define RVMODEL_SET_SEXT_INT(_R1, _R2)
        |#define RVMODEL_CLR_SEXT_INT(_R1, _R2)
        |#define RVMODEL_SET_SSW_INT(_R1, _R2)
        |#define RVMODEL_CLR_SSW_INT(_R1, _R2)
        |
        |#endif // _RVMODEL_MACROS_H
        |""".stripMargin
  }

  def renderRunCommand(m: ActModel): String = {
    val args = ArrayBuffer[String]()
    args += "mill"
    args += "Test[].runMain"
    args += "vexiiriscv.tester.TestBench"
    args += s"--xlen=${m.xlen}"
    args += s"--with-isa=${runIsaList(m).mkString(",")}"
    if (m.param.additionalPerformanceCounters > 0) args += s"--performance-counters=${m.param.additionalPerformanceCounters}"
    if (m.pmpSize > 0) args += s"--pmp-size=${m.pmpSize}"
    if (m.pmpGranularity != 4096) args += s"--pmp-granularity=${m.pmpGranularity}"
    if (m.resetVector != 0x80000000L) args += s"--reset-vector=0x${m.resetVector.toHexString}"
    if (m.param.fetchL1Enable) {
      args += "--fetch-l1"
      args += s"--fetch-l1-ways=${m.param.fetchL1Ways}"
      args += s"--fetch-l1-sets=${m.param.fetchL1Sets}"
      args += s"--fetch-l1-mem-data-width-min=${m.param.fetchMemDataWidthMin}"
    }
    if (m.param.lsuL1Enable) {
      args += "--lsu-l1"
      args += s"--lsu-l1-ways=${m.param.lsuL1Ways}"
      args += s"--lsu-l1-sets=${m.param.lsuL1Sets}"
      args += s"--lsu-l1-mem-data-width-min=${m.param.lsuMemDataWidthMin}"
      args += s"--lsu-l1-refill-count=${m.param.lsuL1RefillCount}"
      args += s"--lsu-l1-writeback-count=${m.param.lsuL1WritebackCount}"
      if (m.param.lsuL1Coherency) args += "--lsu-l1-coherency"
    }
    if (m.param.withLsuBypass) args += "--with-lsu-bypass"
    if (m.param.relaxedBranch) args += "--relaxed-branch"
    if (m.param.allowBypassFrom < 100) args += s"--allow-bypass-from=${m.param.allowBypassFrom}"
    args += "{debug:--with-rvls-log --with-wave --print-stats}"
    args += "--load-elf"
    args.mkString(" ") + "\n"
  }

  def runIsaList(m: ActModel): Seq[String] = {
    m.param.extension.getIsaNameArray().toSeq
  }

  def sailConfigData(m: ActModel): JMap = {
    jmap(
      "$schema" -> "/opt/riscv/share/sail-riscv/sail_riscv_config_schema.json",
      "base" -> jmap(
        "xlen" -> m.xlen,
        "E" -> m.has("e"),
        "writable_misa" -> false,
        "writable_fiom" -> true,
        "writable_hpm_counters" -> jmap("len" -> 32, "value" -> m.hpmMaskHex),
        "scounteren_writable_bits" -> jmap("len" -> 32, "value" -> m.hpmMaskHex),
        "xtval_nonzero" -> exceptionBoolMap(default = true, "reserved_exceptions" -> false),
        "reserved_behavior" -> jmap(
          "amocas_odd_register" -> "AMOCAS_Illegal",
          "fcsr_rm" -> "Fcsr_RM_Illegal",
          "pmpcfg_write_only" -> "PMP_ClearPermissions",
          "xenvcfg_cbie" -> "Xenvcfg_ClearPermissions",
          "xtvec_mode" -> "Xtvec_Ignore",
          "rv32zdinx_odd_register" -> "Zdinx_Illegal"
        )
      ),
      "memory" -> jmap(
        "pmp" -> jmap(
          "grain" -> m.pmpGrain,
          "count" -> m.sailPmpCount,
          "usable_count" -> m.pmpSize,
          "tor_supported" -> m.param.pmpParam.withTor,
          "na4_supported" -> false,
          "napot_supported" -> m.param.pmpParam.withNapot
        ),
        "misaligned" -> jmap(
          "exceptions" -> misalignedExceptions(includeVector = true),
          "allowed_within_exp" -> 0,
          "byte_by_byte" -> false,
          "order_decreasing" -> false
        ),
        "dtb_address" -> sizedHex("0x1000"),
        "regions" -> Seq(
          sailRegion("0x1000", "0x1000", "IOMemory", cacheable = true, coherent = false, executable = false, writable = false, atomic = "AMONone", reservable = "RsrvNone", pte = false),
          sailRegion("0x10000000", "0x10000000", "IOMemory", cacheable = false, coherent = true, executable = false, writable = true, atomic = "AMONone", reservable = "RsrvNone", pte = false),
          sailRegion("0x80000000", "0x80000000", "MainMemory", cacheable = true, coherent = true, executable = true, writable = true, atomic = if (m.hasZaamo) "AMOArithmetic" else "AMONone", reservable = if (m.hasZalrsc) "RsrvEventual" else "RsrvNone", pte = m.param.withMmu)
        )
      ),
      "platform" -> jmap(
        "vendorid" -> 1538,
        "archid" -> 36,
        "impid" -> 256,
        "hartid" -> 0,
        "cache_block_size_exp" -> 6,
        "reservation" -> jmap(
          "reservation_set_size_exp" -> (if (m.xlen == 32) 2 else 3),
          "require_exact_reservation_addr" -> false
        ),
        "clint" -> jmap("base" -> 268500992, "size" -> 49152),
        "simple_interrupt_generator" -> jmap("base" -> 268435456),
        "clock_frequency" -> 1000000000,
        "instructions_per_tick" -> 2,
        "wfi_is_nop" -> false,
        "max_time_to_wait" -> 10
      ),
      "extensions" -> sailExtensionsData(m)
    )
  }

  def exceptionBoolMap(default: Boolean, overrides: (String, Boolean)*): JMap = {
    val ret = jmap(
      "illegal_instruction" -> default,
      "software_breakpoint" -> default,
      "hardware_breakpoint" -> default,
      "load_address_misaligned" -> default,
      "load_access_fault" -> default,
      "load_page_fault" -> default,
      "samo_address_misaligned" -> default,
      "samo_access_fault" -> default,
      "samo_page_fault" -> default,
      "fetch_address_misaligned" -> default,
      "fetch_access_fault" -> default,
      "fetch_page_fault" -> default,
      "software_check" -> default,
      "reserved_exceptions" -> default
    )
    overrides.foreach { case (k, v) => ret.put(k, toJava(v)) }
    ret
  }

  def misalignedExceptions(includeVector: Boolean): JMap = {
    val ret = jmap(
      "load_store" -> jmap("None" -> null),
      "lrsc" -> "AccessFault",
      "amo" -> "AccessFault"
    )
    if (includeVector) ret.put("vector", jmap("None" -> null))
    ret
  }

  def sailExtensionsData(m: ActModel): JMap = {
    val ret = jmap()
    for (ext <- Seq("m", "a", "f", "d", "zicbom", "zicntr", "zicsr", "zifencei", "zihpm", "zknd", "zkne", "sstc")) {
      ret.put(actName(ext), jmap("supported" -> m.has(ext)))
    }
    ret.put("B", jmap("supported" -> m.has("b")))
    ret.put("S", jmap("supported" -> m.has("s")))
    ret.put("U", jmap("supported" -> m.has("u")))
    ret.put("Zmmul", jmap("supported" -> m.has("zmmul")))
    ret.put("Zaamo", jmap("supported" -> m.hasZaamo))
    ret.put("Zalrsc", jmap("supported" -> m.hasZalrsc))
    ret.put("Zca", jmap("supported" -> m.has("zca")))
    ret.put("Zcd", jmap("supported" -> m.has("zcd")))
    ret.put("Zba", jmap("supported" -> m.has("zba")))
    ret.put("Zbb", jmap("supported" -> m.has("zbb")))
    ret.put("Zbs", jmap("supported" -> m.has("zbs")))
    ret.put("Zbc", jmap("supported" -> m.has("zbc")))
    ret.put("Sv32", jmap("supported" -> m.mmuMode.contains("sv32")))
    ret.put("Sv39", jmap("supported" -> m.mmuMode.contains("sv39")))
    ret.put("V", jmap("support_level" -> "Disabled", "vlen_exp" -> 8, "elen_exp" -> 6, "vl_use_ceil" -> false))
    for (ext <- Seq("zic64b", "zicboz", "zicbop", "zicond", "zfa", "zfh", "zfhmin", "zcb", "sscofpmf", "sscounterenw", "sv48", "sv57", "svinval", "svnapot", "svpbmt", "svvptc")) {
      ret.put(actName(ext), jmap("supported" -> false))
    }
    ret.put("Svbare", jmap("supported" -> m.param.withMmu, "sfence_vma_illegal_if_svbare_only" -> true))
    ret.put("Stateen", jmap(
      "Smstateen" -> jmap("supported" -> false),
      "Ssstateen" -> jmap("supported" -> false),
      "C_readonly_zero" -> true,
      "SE0_readonly_zero" -> false
    ))
    ret
  }

  def sailRegion(base: String,
                         size: String,
                         memType: String,
                         cacheable: Boolean,
                         coherent: Boolean,
                         executable: Boolean,
                         writable: Boolean,
                         atomic: String,
                         reservable: String,
                         pte: Boolean): JMap = {
    jmap(
      "base" -> sizedHex(base),
      "size" -> sizedHex(size),
      "attributes" -> jmap(
        "mem_type" -> memType,
        "cacheable" -> cacheable,
        "coherent" -> coherent,
        "executable" -> executable,
        "readable" -> true,
        "writable" -> writable,
        "read_idempotent" -> cacheable,
        "write_idempotent" -> cacheable,
        "misaligned_exceptions" -> misalignedExceptions(includeVector = false),
        "atomic_support" -> atomic,
        "reservability" -> reservable,
        "supports_cbo_zero" -> false,
        "supports_pte_read" -> pte,
        "supports_pte_write" -> pte
      ),
      "include_in_device_tree" -> (memType == "MainMemory")
    )
  }

  def sizedHex(value: String): JMap = jmap("len" -> 64, "value" -> value)

  lazy val yaml = {
    val options = new DumperOptions()
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    options.setPrettyFlow(true)
    new Yaml(options)
  }

  lazy val gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create()

  def renderYaml(data: Object): String = yaml.dump(data)
  def renderJson(data: Object): String = gson.toJson(data) + "\n"

  def jmap(entries: (String, Any)*): JMap = {
    val ret = new LinkedHashMap[String, Object]()
    entries.foreach { case (k, v) => ret.put(k, toJava(v)) }
    ret
  }

  def toJava(value: Any): Object = value match {
    case null => null
    case v: LinkedHashMap[_, _] => v.asInstanceOf[Object]
    case v: ArrayList[_] => v.asInstanceOf[Object]
    case v: Seq[_] => jlist(v)
    case v: String => v
    case v: Boolean => java.lang.Boolean.valueOf(v)
    case v: Int => java.lang.Integer.valueOf(v)
    case v: Long => java.lang.Long.valueOf(v)
    case v: BigInt => v.bigInteger
    case v: Object => v
  }

  def jlist(values: Seq[_]): JList = {
    val ret = new ArrayList[Object]()
    values.foreach(v => ret.add(toJava(v)))
    ret
  }
}
