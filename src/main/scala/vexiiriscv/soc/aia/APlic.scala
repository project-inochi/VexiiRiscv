package vexiiriscv.soc.aia

import spinal.core._
import spinal.lib._
import scala.annotation.meta.param

case class APlicMSIParam(
  base: BigInt = 0,
  hhxs: Int = 0,
  lhxs: Int = 0,
  hhxw: Int = 0,
  lhxw: Int = 0,
)

case class APlicGenParam(withDirect: Boolean,
                         withMSI: Boolean,
                         genIEP: Boolean = true,
                         withIForce: Boolean = false,
                         var _MMsiParams: APlicMSIParam = APlicMSIParam(),
                         var _SMsiParams: APlicMSIParam = APlicMSIParam(),

                         var _withMSIAddrCfg: Boolean = false,
                         var _lockMSI: Boolean = false) {

  def lockMSI(): this.type = {
    this._lockMSI = true
    this
  }

  def withMSIAddrCfg(): this.type = {
    this._withMSIAddrCfg = true
    this
  }

  def withMMSIParams(param: APlicMSIParam): this.type = {
    this._MMsiParams = param
    this
  }

  def withMMSIParams(address: BigInt = 0, hhxs: Int = 0, lhxs: Int = 0, hhxw: Int = 0, lhxw: Int = 0): this.type = {
    withMMSIParams(APlicMSIParam(base = address, hhxs = hhxs, lhxs = lhxs, hhxw = hhxw, lhxw = lhxw))
  }

  def withSMSIParams(param: APlicMSIParam): this.type = {
    this._SMsiParams = param
    this
  }

  def withSMSIParams(address: BigInt = 0, lhxs: Int = 0): this.type = {
    withSMSIParams(APlicMSIParam(base = address, lhxs = lhxs))
  }
}

object APlicGenParam {
  def test = APlicGenParam(
    withDirect  = true,
    withMSI     = true,
    genIEP      = true,
    withIForce  = true,
  )

  def full = APlicGenParam(
    withDirect  = true,
    withMSI     = true,
    genIEP      = true,
  )

  def msi = APlicGenParam(
    withDirect  = false,
    withMSI     = true,
  )

  def direct = APlicGenParam(
    withDirect  = true,
    withMSI     = false,
  )
}

case class APlicDomainParam(isRoot: Boolean,
                            isMDomain: Boolean,
                            genParam: APlicGenParam)

object APlicDomainParam {
  def root(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = true,
    isMDomain = true,
    genParam  = genParam,
  )

  def M(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = false,
    isMDomain = true,
    genParam  = genParam,
  )

  def S(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = false,
    isMDomain = false,
    genParam  = genParam,
  )
}

case class APlicSlaveInfo(childIdx: Int, sourceIds: Seq[Int])

case class APlicMSIPayload() extends Bundle {
  val address = UInt(64 bits)
  val data = UInt(32 bits)
}

trait APlicMSIProducerFiber extends Nameable{
  def createMSIStreamProducer(): Stream[APlicMSIPayload]
}

trait APlicMSIConsumerFiber extends Nameable{
  def createMSIStreamConsumer(): Stream[APlicMSIPayload]
}

case class APlic(p: APlicDomainParam,
                 sourceIds: Seq[Int],
                 hartIds: Seq[Int],
                 slaveInfos: Seq[APlicSlaveInfo]) extends Area {
  require(sourceIds.distinct.size == sourceIds.size, "APlic requires no duplicate interrupt source")
  require(hartIds.distinct.size == hartIds.size, "APlic requires no duplicate harts")

  val sources = Bits(sourceIds.size bits)
  val slaveSources = Vec(slaveInfos.map(slaveInfo => Bits(slaveInfo.sourceIds.size bits)))
  val mmsiaddrcfg = UInt(64 bits)
  val smsiaddrcfg = UInt(64 bits)

  val slaveInterruptIds = slaveInfos.flatMap(slaveInfo => slaveInfo.sourceIds).distinct
  val interruptDelegatable = for (sourceId <- sourceIds) yield slaveInterruptIds.find(_ == sourceId).isDefined

  val deliveryEnable = RegInit(False)
  val isMSI = RegInit(False)
  val bigEndian = False

  if (!p.genParam.withDirect) {
    when (!isMSI) {
      deliveryEnable := False
    }
  }

  val interrupts: Seq[APlicSource] = for (((sourceId, delegatable), i) <- sourceIds.zip(interruptDelegatable).zipWithIndex)
    yield new APlicSource(sourceId, delegatable, isMSI, sources(i))

  val slaveMappings = for ((slaveInfo, slaveSource) <- slaveInfos.zip(slaveSources)) yield new Area {
    for ((slaveSourceId, idx) <- slaveInfo.sourceIds.zipWithIndex) yield new Area {
      interrupts.find(_.id == slaveSourceId).map(interrupt => new Area {
        when(interrupt.delegated && (Bool(slaveInfos.size == 1) || interrupt.childIdx === slaveInfo.childIdx)) {
          slaveSource(idx) := interrupt.input
        } otherwise {
          slaveSource(idx) := False
        }
      })
    }
  }

  val msiaddrcfg = (p.genParam.withMSI || p.genParam._withMSIAddrCfg) generate new Area {
    val M = new Area {
      val (lock, hhxs, lhxs, hhxw, lhxw, ppn) = if (p.isRoot) {
        (RegInit(Bool(p.genParam._lockMSI)),
         RegInit(U(p.genParam._MMsiParams.hhxs, 5 bits)),
         RegInit(U(p.genParam._MMsiParams.lhxs, 3 bits)),
         RegInit(U(p.genParam._MMsiParams.hhxw, 3 bits)),
         RegInit(U(p.genParam._MMsiParams.lhxw, 4 bits)),
         RegInit(U(p.genParam._MMsiParams.base >> 12, 44 bits)))
      } else {
        (mmsiaddrcfg(63),
         mmsiaddrcfg(60 downto 56),
         mmsiaddrcfg(54 downto 52),
         mmsiaddrcfg(50 downto 48),
         mmsiaddrcfg(47 downto 44),
         mmsiaddrcfg(43 downto 0))
      }

      val msiaddrcfg = if (p.isRoot) {
        U(64 bits, 63            -> lock,
                  (60 downto 56) -> hhxs,
                  (54 downto 52) -> lhxs,
                  (50 downto 48) -> hhxw,
                  (47 downto 44) -> lhxw,
                  (43 downto 0)  -> ppn,
                  default        -> False)
      } else {
        mmsiaddrcfg
      }

      val maskH = (U(1) << hhxw) - 1
      val maskL = (U(1) << lhxw) - 1

      val msiaddrcfgCovered = lock.mux(
        True  -> U(64 bits, 63 -> True, default -> False),
        False -> msiaddrcfg,
      )

      if (p.isRoot) {
        mmsiaddrcfg := msiaddrcfg
      }
    }

    val S = new Area {
      val (ppn, lhxs) = if (p.isRoot) {
        (RegInit(U(p.genParam._SMsiParams.base >> 12, 44 bits)),
         RegInit(U(p.genParam._SMsiParams.lhxs, 3 bits)))
      } else {
        (smsiaddrcfg(43 downto 0), smsiaddrcfg(54 downto 52))
      }

      val msiaddrcfg = if (p.isRoot) {
        U(64 bits, 63             -> M.lock,
                   (60 downto 56) -> M.hhxs,
                   (54 downto 52) -> lhxs,
                   (50 downto 48) -> M.hhxw,
                   (47 downto 44) -> M.lhxw,
                   (43 downto 0)  -> ppn,
                   default        -> False)
      } else {
        smsiaddrcfg
      }

      val msiaddrcfgCovered = M.lock.mux(
        True  -> U(0),
        False -> msiaddrcfg,
      )

      if (p.isRoot) {
        smsiaddrcfg := msiaddrcfg
      }
    }

    def msiAddress(hartIndex: UInt, guestIndex: UInt = 0): UInt = {
      val groupId = (hartIndex >> M.lhxw) & M.maskH.resized
      val hartId = hartIndex & M.maskL.resized
      val groupOffset = groupId << (M.hhxs + 12)
      val lhxs = if (p.isMDomain) M.lhxs else S.lhxs
      val ppn = if (p.isMDomain) M.ppn else S.ppn
      val hartOffset = hartId << lhxs

      val msiaddr = (ppn | groupOffset.resized | hartOffset.resized | guestIndex.resized) << 12
      msiaddr
    }
  }

  val msi = p.genParam.withMSI generate new Area {
    val gateway = new APlicMSIGateway(interrupts, deliveryEnable)

    val gatewayStream = gateway.requestStream.map(req => {
      val payload = APlicMSIPayload()
      payload.address := msiaddrcfg.msiAddress(req.target.hartId, req.target.guestId).resized
      payload.data := req.target.eiid.resized
      payload
    })

    val genmsiStream = Stream(APlicMSIPayload())

    val msiStream = StreamArbiterFactory().lowerFirst.noLock.onArgs(gatewayStream, genmsiStream)
  }

  val direct = p.genParam.withDirect generate new Area {
    val gateways = for (hartId <- hartIds) yield new APlicDirectGateway(interrupts, hartId, p.genParam.withIForce)

    val targets = Mux(isMSI, B(0), gateways.map(_.iep && deliveryEnable).asBits())
  }
}

object APlic {
  def doWhenMatch(interrupts: Seq[APlicSource], id: UInt, func: APlicSource => Unit) = new Area {
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          func(interrupt)
        }
      }
    }
  }

  def doClaim(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doClaim())

  def doSet(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doSet())

  def doEnable(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doEnable())

  def doDisable(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doDisable())
}
