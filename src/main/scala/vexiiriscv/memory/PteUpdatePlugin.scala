package vexiiriscv.memory

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv._
import vexiiriscv.riscv.Riscv
import vexiiriscv.misc.PrivilegedPlugin

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}

case class PteUpdate(identity: Int, requestGuest: Boolean, requestLog: Boolean, entryBytes: Int) extends Bundle {
  val cmd = Stream(PteUpdateCmd(requestGuest, requestLog))
  val rsp = Stream(PteUpdateRsp())
}

case class PteUpdateCmd(requestGuest: Boolean, requestLog: Boolean) extends Bundle {
  val address = Global.PHYSICAL_ADDRESS()
  val expected = Bits(Riscv.XLEN bits)
  val guest = requestGuest generate Bool()
  val mask = new Bundle {
    val A = Bool()
    val D = Bool()
  }
  val virtualPPN = requestLog generate UInt(Global.MIXED_WIDTH - 12 bits)
}

case class PteUpdateRsp() extends Bundle {
  val data = Bits(Riscv.XLEN bits)
  val error = Bits(2 bits)
  val implicitWrite = Bool()
  val redo = Bool()
  val logFault = Bool()
}

trait PteUpdateLog {
  def recorded(address: UInt, guest: Bool): Bool
  def data(address: UInt, guest: Bool): Bits
  def full(address: UInt, guest: Bool): Bool
  def bufferAddress(address: UInt, guest: Bool): UInt
  def indexIncrease(address: UInt, guest: Bool): Unit
}

case class DefaultPteUpdateLog(entryBytes: Int, requestGuest: Boolean = false) extends Area with PteUpdateLog {
  val entryWidth = log2Up(entryBytes)
  val pageShift = 12 - entryWidth

  case class LogState() extends Bundle {
    val enable = RegInit(False)
    val address = RegInit(U(0, Global.MIXED_WIDTH - 12 bits))

    val counter = RegInit(U(0, 20 bits))
    val size = RegInit(U(0, 4 bits))
    val counterSize = U(1, 20 bits) |<< (size + pageShift)

    val full = counter >= counterSize

    def counterInc() = counter := counter + 1
    def bufferAddress = ((address << 12) | (counter << entryWidth).resize(Global.MIXED_WIDTH.get))
  }

  val state = new LogState()
  val guestState = requestGuest generate new LogState()

  def targetState[T <: Data](guest: Bool, f: LogState => T) = requestGuest.mux(guest.mux(f(guestState), f(state)), f(state))
  def recorded(gppn: UInt, guest: Bool): Bool = targetState(guest, _.enable)
  def data(gppn: UInt, guest: Bool): Bits = (gppn << 12).asBits.resized
  def full(gppn: UInt, guest: Bool): Bool = targetState(guest, _.full)
  def bufferAddress(gppn: UInt, guest: Bool): UInt = targetState(guest, _.bufferAddress).resized
  def indexIncrease(gppn: UInt, guest: Bool): Unit = {
    val incCtx = WhenBuilder()
    if (requestGuest) incCtx.when(guest) {
      guestState.counterInc()
    }
    incCtx.otherwise {
      state.counterInc()
    }
  }
}

class PteUpdatePlugin extends FiberPlugin {
  val updaters = ArrayBuffer[PteUpdate]()
  def newPteUpdate(identity: Int, entryBytes: Int, requestGuest: Boolean = false, recordLog: Boolean = false) = updaters.addRet(new PteUpdate(identity, requestGuest, recordLog, entryBytes))
  val loggers = LinkedHashMap[Int, PteUpdateLog]()
  def registerPteLog(identity: Int, callback: PteUpdateLog) = loggers.addRet(identity -> callback)

  val accessRetainer = Retainer()

  val logic = during setup new Area{
    val priv = host[PrivilegedPlugin]
    val access = host[TranslatedDBusAccessService]

    val accessLock = retains(access.accessRetainer)

    awaitBuild()

    accessRetainer.await()

    assert(updaters.map(_.identity).distinct.size == updaters.size)
    val updateWithLogs = updaters.filter(_.requestLog).map(_.identity)
    assert(loggers.filterKeys(updateWithLogs.contains(_)).size == updateWithLogs.size)

    val fsm = for (update <- updaters) yield new StateMachine {
      val IDLE, CMD, RSP = new State
      val LOG_CHECK, LOG_CMD, LOG_RSP = new State
      setEntry(IDLE)

      val logger = update.requestLog generate loggers(update.identity)

      val request = Reg(PteUpdateCmd(update.requestGuest, update.requestLog))
      val target = Reg(Bits(Riscv.XLEN bits))
      val currentGuest = update.requestGuest.mux(request.guest, False)

      val updateBus = access.newDBusUpdate(update.requestGuest)

      def ucmd = updateBus.cmd
      val ursp = updateBus.rsp.toStream.stage()

      ucmd.valid    := False
      ucmd.address  := request.address
      ucmd.expected := request.expected
      ucmd.cas      := False
      ucmd.data     := target
      ucmd.size     := U(log2Up(update.entryBytes))
      if (update.requestGuest) ucmd.guest := request.guest

      ursp.ready    := False

      def cmd = update.cmd
      def rsp = update.rsp

      cmd.ready         := False
      rsp.valid         := False
      rsp.data          := ursp.data
      rsp.error         := ursp.error
      rsp.implicitWrite := ursp.implicitWrite
      rsp.redo          := !(ursp.error.orR || ursp.updated || ursp.dirtyLogFault)
      rsp.logFault      := False

      IDLE whenIsActive {
        when (cmd.valid) {
          request   := cmd.payload
          target    := cmd.expected
          target(6).setWhen(cmd.mask.A)
          target(7).setWhen(cmd.mask.D)
          cmd.ready := True

          val logCtx = WhenBuilder()
          if (update.requestLog) logCtx.when(logger.recorded(cmd.virtualPPN, currentGuest) && cmd.mask.D) {
            goto(LOG_CHECK)
          }
          logCtx.otherwise {
            goto(CMD)
          }
        }
      }

      val log = update.requestLog generate new Area {
        LOG_CHECK whenIsActive {
          when (logger.full(request.virtualPPN, currentGuest)) {
            rsp.valid         := True
            rsp.data          := request.expected
            rsp.redo          := False
            rsp.error         := B(0)
            rsp.implicitWrite := False
            rsp.logFault      := True
            when (rsp.ready) {
              goto(IDLE)
            }
          } otherwise {
            goto(LOG_CMD)
          }
        }

        LOG_CMD whenIsActive {
          ucmd.valid    := True
          ucmd.address  := logger.bufferAddress(request.virtualPPN, currentGuest)
          ucmd.cas      := False
          ucmd.data     := logger.data(request.virtualPPN, currentGuest)

          when (ucmd.ready) {
            goto(LOG_RSP)
          }
        }

        LOG_RSP whenIsActive {
          when (ursp.valid) {
            when (ursp.error.orR) {
              rsp.valid  := True
              when (rsp.ready) {
                ursp.ready := True
                goto(IDLE)
              }
            } otherwise {
              ursp.ready := True
              goto(CMD)
            }
          }
        }
      }

      CMD whenIsActive {
        ucmd.valid    := True
        ucmd.cas      := True
        when (ucmd.ready) {
          goto(RSP)
        }
      }

      RSP whenIsActive {
        when (ursp.valid) {
          rsp.valid := True
          rsp.logFault := ursp.dirtyLogFault
          val updated = !(ursp.error.orR || ursp.dirtyLogFault) && ursp.updated
          when (updated) {
            rsp.data(6, 2 bits) := target(6, 2 bits)
          }
          when (rsp.ready) {
            if (update.requestLog) when (logger.recorded(request.virtualPPN, request.guest) && request.mask.D && updated) {
              logger.indexIncrease(request.virtualPPN, request.guest)
            }
            ursp.ready := True
            goto(IDLE)
          }
        }
      }
    }

    accessLock.release()
  }
}
