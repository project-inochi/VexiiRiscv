package vexiiriscv.memory

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{NodeBaseApi, Payload}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.execute.{CsrAccessPlugin, CsrListFilter, CsrRamService}
import vexiiriscv.misc.{PipelineBuilderPlugin, PrivilegedPlugin, TrapReason}
import vexiiriscv.riscv.{CSR, Riscv}

import scala.collection.mutable.ArrayBuffer

/**
 * Reading the RISC-V spec to figure out all the flags/details of the PMP is just too tricky.
 * A good way to figure things out is to look at spike (software implementation) :
 * - https://github.com/riscv-software-src/riscv-isa-sim/blob/2c67071743d4b55719cee22fdb319df2a0756db7/riscv/mmu.cc#L348
 * - https://github.com/riscv-software-src/riscv-isa-sim/blob/2c67071743d4b55719cee22fdb319df2a0756db7/riscv/csrs.cc#L183
 * - https://github.com/riscv-software-src/riscv-isa-sim/blob/2c67071743d4b55719cee22fdb319df2a0756db7/riscv/mmu.cc#L348
 *
 * In particular, one tricky thing with the PMP is how the PMPADDRx LSB bits are handled in regard of the granularity.
 * Their value change depending the PMPCFGx XD
 */


case class PmpStorageParameter(slots : Int)
case class PmpPortParameter(var napotMatchAt : Int,
                            var napotHitsAt : Int,
                            var torCmpAt : Int,
                            var torHitsAt : Int,
                            var hitsAt : Int,
                            var rspAt : Int) {
  def offset(v: Int) = PmpPortParameter(
    napotMatchAt = napotMatchAt  + v,
    napotHitsAt  = napotHitsAt   + v,
    torCmpAt     = torCmpAt      + v,
    torHitsAt    = torHitsAt     + v,
    hitsAt       = hitsAt        + v,
    rspAt        = rspAt         + v
  )
}


case class PmpParam(
  var pmpSize: Int,
  var granularity: Int,
  var withTor: Boolean = true,
  var withNapot: Boolean = true,
  var withSmepmp: Boolean = false
)

/**
 * This PMP implementation is quite regular, but allows the user to get ports which are pipelined (to improve FMax / timings)
 */
class PmpPlugin(val p : PmpParam) extends FiberPlugin with PmpService{
  import p._

  override def getPmpNum(): Int = p.pmpSize

  assert(isPow2(granularity))
  val granularityWidth = log2Up(granularity)

  case class PortSpec(stages: Seq[NodeBaseApi],
                      physicalAddress: Payload[UInt],
                      size: NodeBaseApi => UInt,
                      forceCheck: NodeBaseApi => Bool,
                      read: NodeBaseApi => Bool,
                      write: NodeBaseApi => Bool,
                      execute: NodeBaseApi => Bool,
                      pp: PmpPortParameter,
                      rsp : PmpRsp){
    val permStage = stages(0)
    val napotMatchStage = stages(pp.napotMatchAt)
    val napotHitsStage = stages(pp.napotHitsAt)
    val torCmpStage = stages(pp.torCmpAt)
    val torHitsStage = stages(pp.torHitsAt)
    val hitsStage = stages(pp.hitsAt)
    val rspStage  = stages(pp.rspAt)
  }
  val portSpecs = ArrayBuffer[PortSpec]()

  override def createPmpPort(nodes: Seq[NodeBaseApi],
                             physicalAddress: Payload[UInt],
                             size: NodeBaseApi => UInt,
                             forceCheck: NodeBaseApi => Bool,
                             read: NodeBaseApi => Bool,
                             write: NodeBaseApi => Bool,
                             execute: NodeBaseApi => Bool,
                             portSpec: Any,
                             storageSpec: Any): PmpRsp = {
    val pp = portSpec.asInstanceOf[PmpPortParameter]
    portSpecs.addRet(
      new PortSpec(
        stages          = nodes,
        physicalAddress = physicalAddress,
        size            = size,
        forceCheck      = forceCheck,
        read            = read,
        write           = write,
        execute         = execute,
        pp              = pp,
        rsp             = new PmpRsp()
      )
    ).rsp
  }

  val logic = during setup new Area{
    val priv = host[PrivilegedPlugin]
    val csr = host[CsrAccessPlugin]
    val ram = host[CsrRamService]

    val csrLock = retains(csr.csrLock, ram.csrLock)
    val buildBefore = retains(List(host[PipelineBuilderPlugin].elaborationLock))

    awaitBuild()

    if(Riscv.XLEN.get == 64) assert(
      granularity >= 8,
      """VexiiRiscv RV64 doesn't support granularity smaller than 8 bytes, as durring 8 bytes accesses,
        | it would need to check that the upper 4 bytes of access are contained in the pmp regions aswell""".stripMargin
    )

    case class Cfg() extends Bundle{
      val kind = Bits(2 bits)
      val read, write, execute = Bool()
      val locked = Bool()
    }

    val pRange = Global.PHYSICAL_WIDTH-1 downto granularityWidth
    // This is a tricky thing, basicaly the granularity doesn't affect TOR and NAPOT csr in the same way.
    // In particular the granularityWidth-2 bit
    val extraBit = granularity > 4

    val mseccfg = new Area {
      val mml  = if(p.withSmepmp) RegInit(False) else False
      val mmwp = if(p.withSmepmp) RegInit(False) else False
      val rlb  = if(p.withSmepmp) RegInit(False) else False
    }

    // Generate all the PMP entries storage + CSR mapping
    val entries = for(i <- 0 until pmpSize) yield new Area{
      val isLocked = Bool()
      val address = Reg(UInt(Global.PHYSICAL_WIDTH.get - granularityWidth + extraBit.toInt bits))
      val cfg = Reg(Cfg())
      val cfgNext = CombInit(cfg) // This allows to handle WARL (Write Any, Read Legal) on the CSRs
      val cfgWriteLocked = cfg.locked && !mseccfg.rlb
      val cfgWriteForbidden = False
      if(p.withSmepmp) {
        // Smepmp forbids creating locked executable M-only or locked shared-executable rules unless RLB is set.
        cfgWriteForbidden setWhen(mseccfg.mml && !mseccfg.rlb && cfgNext.locked &&
          !(cfgNext.execute && cfgNext.write && cfgNext.read) &&
          (cfgNext.execute || (cfgNext.write && !cfgNext.read)))
      }
      when(!cfgWriteLocked && !cfgWriteForbidden) {
        cfg := cfgNext
        if(p.withSmepmp) {
          when(!mseccfg.mml) {
            cfg.write clearWhen (!cfgNext.read)
          }
        } else {
          cfg.write clearWhen (!cfgNext.read)
        }
        if(!p.withTor) when(cfgNext.kind === 1) {cfg.kind := 0}
        if(!p.withNapot){
          if(granularity > 4) when(cfgNext.kind === 2) {cfg.kind := 0}
          when(cfgNext.kind === 3) {cfg.kind := 0}
        }
      }

      if(i == 0){
        // Allows software which is unaware of the PMP but uses the supervisor/user modes
        // to get access to all the memory by default after reset
        assert(p.withNapot)
        address.init(address.getAllTrue)
        cfg.read init(True)
        cfg.write init(True)
        cfg.execute init(True)
        cfg.kind init(3)
        cfg.locked init(False)
      } else {
        address.init(0)
        cfg.initZero()
      }


      val isNapot = RegNext(cfg.kind(1))
      val isTor = RegNext(cfg.kind === 1)
      val napot = Napot(address.dropHigh(1)).dropLow(extraBit.toInt)
      if(granularity == 4) when(!cfg.kind(0)){ napot.setAll() }
      val mask = RegNext(napot)

      val (cfgId, cfgOffset) = Riscv.XLEN.get match{
        case 32 => (i/4, i%4*8)
        case 64 => (i/8*2, i%8*8)
      }

      csr.writeCancel(CSR.PMPADDR + i, isLocked && !mseccfg.rlb)
      csr.read(address(address.bitsRange.drop(extraBit.toInt)), CSR.PMPADDR + i, granularityWidth-2)
      csr.write(address(address.bitsRange), CSR.PMPADDR + i, granularityWidth-2-extraBit.toInt)
      if(granularity > 4 && p.withNapot) csr.read(U(granularityWidth-2 bits, default -> isNapot, granularityWidth-3 -> (address.lsb && isNapot)), CSR.PMPADDR + i) //WTF
      csr.read(CSR.PMPCFG + cfgId,  0+cfgOffset -> cfg.read, 1+cfgOffset -> cfg.write, 2+cfgOffset -> cfg.execute, 3+cfgOffset -> cfg.kind, 7+cfgOffset -> cfg.locked)
      csr.write(CSR.PMPCFG + cfgId,  0+cfgOffset -> cfgNext.read, 1+cfgOffset -> cfgNext.write, 2+cfgOffset -> cfgNext.execute, 3+cfgOffset -> cfgNext.kind, 7+cfgOffset -> cfgNext.locked)
    }

    for(i <- 0 until pmpSize; self = entries(i)) {
      self.isLocked := self.cfg.locked || (i+1 != pmpSize).mux(entries(i+1).cfg.locked && entries(i+1).cfg.kind === 1, False)
    }

    if(p.withSmepmp) {
      val read = Bits(Riscv.XLEN bits)
      read := 0
      read(0) := mseccfg.mml
      read(1) := mseccfg.mmwp
      read(2) := mseccfg.rlb
      csr.read(read, CSR.MSECCFG)
      csr.onWrite(CSR.MSECCFG, true) {
        val anyLocked = entries.map(_.cfg.locked).orR
        when(!(anyLocked && !mseccfg.rlb)) {
          mseccfg.rlb := csr.bus.write.bits(2)
        }
        mseccfg.mml  setWhen(csr.bus.write.bits(0))
        mseccfg.mmwp setWhen(csr.bus.write.bits(1))
      }
      csr.onDecode(CSR.MSECCFG) {
        when(csr.bus.decode.write) {
          csr.bus.decode.doTrap(TrapReason.NEXT)
        }
      }
      if(Riscv.XLEN.get == 32) {
        csr.read(B(0, 32 bits), CSR.MSECCFGH)
      }
    }

    val allFilter = CsrListFilter((0 to 15).map(_ + CSR.PMPADDR) ++ (0 to 3).filter(i => Riscv.XLEN.get == 32 || (i % 2) == 0).map(_ + CSR.PMPCFG))
    if(p.pmpSize > 0) csr.onDecode(allFilter) {
      when(csr.bus.decode.write) {
        csr.bus.decode.doTrap(TrapReason.NEXT)
      }
    }

    assert(Global.HART_COUNT.get == 1)

    csrLock.release()
    portsLock.await()

    val isMachine = priv.isMachine(0)
    val instructionShouldHit = !isMachine
    val dataMprvNonMachine = priv.logic.harts(0).m.status.mprv && priv.logic.harts(0).m.status.mpp =/= 3
    val dataShouldHit = !isMachine || dataMprvNonMachine
    val dataIsMachine = isMachine && !dataMprvNonMachine

    val ports = for(ps <- portSpecs) yield new Composite(ps.rsp, "logic", false){
      import ps._
      val dataShouldHitPort = dataShouldHit || ps.forceCheck(permStage)
      val dataIsMachinePort = dataIsMachine && !ps.forceCheck(permStage)
      val accessSize = ps.size(permStage)
      val accessSizeWidth = widthOf(accessSize)
      val accessMask = accessSize.muxListDc((0 until (1 << accessSizeWidth)).map(i =>
        U(i, accessSizeWidth bits) -> U((BigInt(1) << i) - 1, Global.PHYSICAL_WIDTH bits)
      ))
      val ACCESS_LAST = permStage.insert((permStage(ps.physicalAddress) + accessMask).resized)
      val ACCESS_ACTIVE = permStage.insert(ps.execute(permStage) || ps.read(permStage) || ps.write(permStage))
      val accessIsData = ps.read(permStage) || ps.write(permStage)
      val accessIsMachine = Mux(accessIsData, dataIsMachinePort, isMachine)
      val TYPE_WRITE = ps.write(permStage)
      val TYPE_EXECUTE = ps.execute(permStage) && !ps.read(permStage) && !ps.write(permStage)
      val TYPE_HLVX = ps.execute(permStage) && ps.read(permStage) && !ps.write(permStage)
      val TYPE_READ = ps.read(permStage) && !ps.write(permStage)
      val needHit = instructionShouldHit && ps.execute(permStage) || dataShouldHitPort && (ps.read(permStage) || ps.write(permStage))
      val NEED_HIT = permStage.insert(needHit)
      val NO_MATCH_OK = permStage.insert {
        if(p.withSmepmp) {
          Mux(mseccfg.mml,
            accessIsMachine && !mseccfg.mmwp && (TYPE_READ || TYPE_WRITE),
            accessIsMachine && !mseccfg.mmwp
          )
        } else {
          !needHit
        }
      }
      val torCmpAddress = torCmpStage(ps.physicalAddress) >> granularityWidth
      val torLastCmpAddress = torCmpStage(ACCESS_LAST) >> granularityWidth
      val TOR_SMALLER = new Array[Payload[Bool]](p.pmpSize)
      val TOR_LAST_SMALLER = new Array[Payload[Bool]](p.pmpSize)
      val onEntries = for((e, id) <- entries.zipWithIndex) yield new Area{
        val napot = p.withNapot generate new Area {
          val MATCH = napotMatchStage.insert(((e.address.dropLow(extraBit.toInt).asUInt << granularityWidth)(pRange) ^ napotMatchStage(ps.physicalAddress)(pRange)).asBits & e.mask)
          val LAST_MATCH = napotMatchStage.insert(((e.address.dropLow(extraBit.toInt).asUInt << granularityWidth)(pRange) ^ napotMatchStage(ACCESS_LAST)(pRange)).asBits & e.mask)
          val HIT = napotHitsStage.insert(napotHitsStage(MATCH) === 0)
          val LAST_HIT = napotHitsStage.insert(napotHitsStage(LAST_MATCH) === 0)
        }
        val tor = p.withTor generate new Area{
          val BIGGER = torCmpStage.insert((e.address >> extraBit.toInt) > torCmpAddress)
          val LAST_BIGGER = torCmpStage.insert((e.address >> extraBit.toInt) > torLastCmpAddress)
          val HIT = { import torHitsStage._ ; insert(BIGGER && (id != 0).mux(!TOR_SMALLER(id-1), True))}
          val LAST_HIT = { import torHitsStage._ ; insert(LAST_BIGGER && (id != 0).mux(!TOR_LAST_SMALLER(id-1), True))}
          TOR_SMALLER(id) = BIGGER
          TOR_LAST_SMALLER(id) = LAST_BIGGER
        }
        val HIT_START = { import hitsStage._ ; insert(p.withNapot.mux(e.isNapot && napot.HIT, False) || p.withTor.mux(e.isTor && tor.HIT, False)) }
        val HIT_END = { import hitsStage._ ; insert(p.withNapot.mux(e.isNapot && napot.LAST_HIT, False) || p.withTor.mux(e.isTor && tor.LAST_HIT, False)) }
        val HIT_ANY = { import hitsStage._ ; insert(HIT_START || HIT_END) }
        val HIT_FULL = { import hitsStage._ ; insert(HIT_START && HIT_END) }

        val instructionCheck = e.cfg.locked || instructionShouldHit
        val dataCheck = e.cfg.locked || dataShouldHitPort
        val legacyRwx = (!ps.execute(permStage) || e.cfg.execute || !instructionCheck) &&
                        (((!ps.write(permStage) || e.cfg.write) && (!ps.read(permStage) || e.cfg.read)) || !dataCheck)
        val smepmpRwx = TYPE_READ && e.cfg.read && (!TYPE_HLVX || e.cfg.execute) ||
                        TYPE_WRITE && e.cfg.write ||
                        TYPE_EXECUTE && e.cfg.execute
        val smepmpLockedSharedData = e.cfg.execute && e.cfg.write && e.cfg.read && e.cfg.locked
        val smepmpSharedRegion = !e.cfg.read && e.cfg.write
        val smepmpNormal = (accessIsMachine === e.cfg.locked) && smepmpRwx
        val smepmpShared =
          !e.cfg.locked && e.cfg.execute && (TYPE_READ || TYPE_WRITE) ||
          !e.cfg.locked && !e.cfg.execute && (TYPE_READ || (TYPE_WRITE && accessIsMachine)) ||
          e.cfg.locked && TYPE_EXECUTE ||
          e.cfg.locked && TYPE_READ && e.cfg.execute && accessIsMachine
        val smepmpPerm = Mux(smepmpLockedSharedData, TYPE_READ && !TYPE_HLVX, Mux(smepmpSharedRegion, smepmpShared, smepmpNormal))
        val PERM_OK = permStage.insert(if(p.withSmepmp) Mux(mseccfg.mml, smepmpPerm, legacyRwx) else legacyRwx)
      }

      val onCtrl = (p.pmpSize > 0) generate new Area{
        import ps.rspStage._
        val hits = Cat(onEntries.map(e => ps.rspStage(e.HIT_ANY)))
        val oh = OHMasking.firstV2(hits)
        val hitAny = hits.orR
        val reader = onEntries.reader(oh)
        val full = reader(e => ps.rspStage(e.HIT_FULL))
        val permOk = reader(e => ps.rspStage(e.PERM_OK))

        rsp.ACCESS_FAULT := ACCESS_ACTIVE && Mux(hitAny, !(full && permOk), !NO_MATCH_OK)
      }
      if(p.pmpSize == 0) ps.rspStage(rsp.ACCESS_FAULT) := False
    }


    buildBefore.release()
  }
}
