package vexiiriscv.memory

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import vexiiriscv._
import Global._
import vexiiriscv.riscv.Riscv
import vexiiriscv.misc.PrivilegedPlugin

import scala.collection.mutable.ArrayBuffer

case class PteUpdate(identity: Int, requestGuest: Boolean, entryBytes: Int) extends Bundle {
  val cmd = Stream(PteUpdateCmd(requestGuest))
  val rsp = Stream(PteUpdateRsp())
}

case class PteUpdateCmd(requestGuest: Boolean) extends Bundle {
  val address = Global.PHYSICAL_ADDRESS()
  val expected = Bits(Riscv.XLEN bits)
  val guest = requestGuest generate Bool()
  val mask = new Bundle {
    val A = Bool()
    val D = Bool()
  }
}

case class PteUpdateRsp() extends Bundle {
  val data = Bits(Riscv.XLEN bits)
  val error = Bits(2 bits)
  val implicitWrite = Bool()
  val redo = Bool()
}

class PteUpdatePlugin extends FiberPlugin {
  val updaters = ArrayBuffer[PteUpdate]()
  def newPteUpdate(identity: Int, requestGuest: Boolean, entryBytes: Int) = updaters.addRet(new PteUpdate(identity, requestGuest, entryBytes))

  val accessRetainer = Retainer()

  val logic = during setup new Area{
    val priv = host[PrivilegedPlugin]
    val access = host[TranslatedDBusAccessService]

    val accessLock = retains(access.accessRetainer)

    awaitBuild()

    accessRetainer.await()

    assert(updaters.map(_.identity).distinct.size == updaters.size)

    val fsm = for (update <- updaters) yield new StateMachine {
      val IDLE, CMD, RSP = new State
      setEntry(IDLE)

      val request = Reg(PteUpdateCmd(update.requestGuest))
      val target = Reg(Bits(Riscv.XLEN bits))

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
      rsp.redo          := !(ursp.error.orR || ursp.updated)

      IDLE whenIsActive {
        when (cmd.valid) {
          request   := cmd.payload
          target    := cmd.expected
          target(6).setWhen(cmd.mask.A)
          target(7).setWhen(cmd.mask.D)
          cmd.ready := True
          goto(CMD)
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
          when (!ursp.error.orR && ursp.updated) {
            rsp.data(6, 2 bits) := target(6, 2 bits)
          }
          when (rsp.ready) {
            ursp.ready := True
            goto(IDLE)
          }
        }
      }
    }

    accessLock.release()
  }
}
