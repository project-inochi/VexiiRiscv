package vexiiriscv.soc.aia

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import spinal.lib.misc.plic.InterruptCtrlFiber
import scala.collection.mutable.ArrayBuffer

class MappedAplic[T <: spinal.core.Data with IMasterSlave](
  sourceIds: Seq[Int],
  hartIds: Seq[Int],
  slaveInfos: Seq[APlicSlaveInfo],
  p: APlicDomainParam,
  slaveType: HardType[T],
  factoryGen: T => BusSlaveFactory,
) extends Component {
  require(sourceIds.distinct.size == sourceIds.size, "APlic requires no duplicate interrupt source")
  require(hartIds.distinct.size == hartIds.size, "APlic requires no duplicate harts")

  val io = new Bundle {
    val slaveBus = slave(slaveType())
    val sources = in Bits (sourceIds.size bits)
    val mmsiaddrcfg = (if (p.isRoot) out else in) UInt (64 bits)
    val smsiaddrcfg = (if (p.isRoot) out else in) UInt (64 bits)
    val targets = p.genParam.withDirect generate (out Bits (hartIds.size bits))
    val slaveSources = out Vec(slaveInfos.map(slaveInfo => Bits(slaveInfo.sourceIds.size bits)))
    val msiMsg = p.genParam.withMSI generate master(Stream(APlicMSIPayload()))
  }

  if (p.isRoot && (p.genParam.withMSI || p.genParam._withMSIAddrCfg)) {
    io.mmsiaddrcfg.assignDontCare()
    io.smsiaddrcfg.assignDontCare()
  }

  val aplic = APlic(p, sourceIds, hartIds, slaveInfos)

  aplic.sources := io.sources
  if (p.genParam.withDirect) {
    io.targets := aplic.direct.targets
  }
  io.slaveSources := aplic.slaveSources

  if (p.isRoot) {
    io.mmsiaddrcfg := aplic.mmsiaddrcfg
    io.smsiaddrcfg := aplic.smsiaddrcfg
  } else {
    aplic.mmsiaddrcfg := io.mmsiaddrcfg
    aplic.smsiaddrcfg := io.smsiaddrcfg
  }

  if (p.genParam.withMSI) {
    io.msiMsg << aplic.msi.msiStream
  }

  val factory = factoryGen(io.slaveBus)
  val mapping = APlicMapper(factory, p)(aplic)
}

case class TilelinkAplic(sourceIds: Seq[Int], hartIds: Seq[Int], slaveInfos: Seq[APlicSlaveInfo],
                         domainParam: APlicDomainParam, params: tilelink.BusParameter)
                         extends MappedAplic(
  sourceIds,
  hartIds,
  slaveInfos,
  domainParam,
  new bus.tilelink.Bus(params),
  new bus.tilelink.SlaveFactory(_, true)
)

case class TilelinkAPLICMSISender(mastersParams: tilelink.BusParameter) extends Component {
  val io = new Bundle {
    val msiMsg = slave(Stream(APlicMSIPayload()))
    val bus = master(tilelink.Bus(mastersParams))
  }

  val out = io.msiMsg.map(payload => {
    val channelA = cloneOf(io.bus.a.payloadType)
    channelA.opcode   := tilelink.Opcode.A.PUT_FULL_DATA
    channelA.size     := 2
    channelA.source   := 0
    channelA.address  := payload.address.resized
    channelA.data     := payload.data.asBits.resized
    channelA.debugId  := 0
    channelA.mask     := 0xf
    channelA
  })

  io.bus.a <-< out
  io.bus.d.ready := True
}

case class TilelinkAPlicMasterParam(addressWidth: Int, pendingSize: Int)

object TilelinkAplic {
  def getTilelinkSlaveSupport(proposed: bus.tilelink.M2sSupport, addressWidth: Int = 20) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )

  def getTilelinkMasterSupport(param: TilelinkAPlicMasterParam, name: Nameable) = bus.tilelink.M2sParameters(
    addressWidth = param.addressWidth,
    dataWidth = 32,
    masters = List(
      tilelink.M2sAgent(
        name = name,
        mapping = List(
          tilelink.M2sSource(
            id = SizeMapping(0, param.pendingSize),
            emits = tilelink.M2sTransfers(
              putFull = tilelink.SizeRange(4)
            )
          )
        )
      )
    )
  )

  def addressWidth(maxTargetId: Int): Int = {
    import APlicMapping._
    val aplicSize = idcOffset + maxTargetId * idcGroupSize
    return log2Up(aplicSize)
  }
}

case class TilelinkAPLICMSISenderFiber() extends Area with APlicMSIConsumerFiber {
  val node = tilelink.fabric.Node.down()
  var msiStream: Option[Stream[APlicMSIPayload]] = None

  val m2sParams = TilelinkAplic.getTilelinkMasterSupport(TilelinkAPlicMasterParam(64, 4), TilelinkAPLICMSISenderFiber.this)

  override def createMSIStreamConsumer(): Stream[APlicMSIPayload] = {
    if (msiStream.isEmpty) {
      msiStream = Some(Stream(APlicMSIPayload()))
    }

    msiStream.get
  }

  val thread = Fiber build new Area {
    node.m2s forceParameters m2sParams
    node.s2m.supported load tilelink.S2mSupport.none()

    val core = TilelinkAPLICMSISender(node.bus.p)

    core.io.bus <> node.bus
    core.io.msiMsg << msiStream.get
  }
}

case class TilelinkAPLICFiber() extends Area with InterruptCtrlFiber with APlicMSIProducerFiber {
  val node = tilelink.fabric.Node.up()
  val core = Handle[TilelinkAplic]()

  case class SourceSpec(node: InterruptNode, id: Int)
  case class TargetSpec(node: InterruptNode, id: Int)
  case class APlicSlaveBundle(slaveInfo: APlicSlaveInfo) extends Area {
    val flags = slaveInfo.sourceIds.map(_ => InterruptNode.master())
  }

  val sources = ArrayBuffer[SourceSpec]()
  val targets = ArrayBuffer[TargetSpec]()
  var domainParam: Option[APlicDomainParam] = None
  var msiStream: Option[Stream[APlicMSIPayload]] = None
  val mmsiaddrcfg = UInt (64 bits)
  val smsiaddrcfg = UInt (64 bits)

  override def createMSIStreamProducer(): Stream[APlicMSIPayload] = {
    if (msiStream.isEmpty) {
      msiStream = Some(Stream(APlicMSIPayload()))
    }

    msiStream.get
  }

  override def createInterruptMaster(id : Int) : InterruptNode = {
    val spec = node.clockDomain on TargetSpec(InterruptNode.master(), id)
    targets += spec
    spec.node
  }

  override def createInterruptSlave(id: Int) : InterruptNode = {
    val spec = node.clockDomain on SourceSpec(InterruptNode.slave(), id)
    sources += spec
    spec.node
  }

  val slaveSources = ArrayBuffer[APlicSlaveBundle]()
  def createInterruptDelegation(slaveInfo: APlicSlaveInfo) = {
    slaveSources.addRet(APlicSlaveBundle(slaveInfo))
  }

  val thread = Fiber build new Area {
    lock.await()

    val p = domainParam.get

    node.m2s.supported.load(TilelinkAplic.getTilelinkSlaveSupport(node.m2s.proposed, TilelinkAplic.addressWidth(targets.map(_.id).max + 1)))
    node.s2m.none()

    val aplic = TilelinkAplic(sources.map(_.id).toSeq, targets.map(_.id).toSeq, slaveSources.map(_.slaveInfo).toSeq, p, node.bus.p)

    core.load(aplic)

    if (p.genParam.withMSI) {
      msiStream.get << core.io.msiMsg
    }

    core.io.slaveBus <> node.bus
    core.io.sources := sources.map(_.node.flag).asBits()
    if (p.genParam.withDirect) {
      Vec(targets.map(_.node.flag)) := core.io.targets.asBools
    }

    if (p.isRoot) {
      mmsiaddrcfg := core.io.mmsiaddrcfg
      smsiaddrcfg := core.io.smsiaddrcfg
    } else {
      core.io.mmsiaddrcfg := mmsiaddrcfg
      core.io.smsiaddrcfg := smsiaddrcfg
    }

    slaveSources.lazyZip(core.io.slaveSources).foreach((slaveSource, ioSlaveSource) => {
      Vec(slaveSource.flags.map(_.flag)) := ioSlaveSource.asBools
    })
  }
}
