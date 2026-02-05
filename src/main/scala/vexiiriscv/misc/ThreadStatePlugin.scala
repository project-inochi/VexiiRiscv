package vexiiriscv.misc

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global._
import vexiiriscv.riscv._

/**
  * This plugin is used to provide global state for each thread.
  *
  */
class ThreadStatePlugin extends FiberPlugin {
  def hart(id : Int) = logic.harts(id)

  def getPrivilege(hartId : UInt) : SInt = logic.harts.map(_.privilege).read(hartId)
  def isMachine(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.M
  def isSupervisor(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.S
  def isUSer(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.U
  def isVirtualSupervisor(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.VS
  def isVirtualUSer(hartId : UInt) : Bool = getPrivilege(hartId) === PrivilegeMode.VU

  val logic = during setup new Area {
    awaitBuild()

    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val privilege = Reg(PrivilegeMode.TYPE()) init(PrivilegeMode.M)
      val withMachinePrivilege = privilege >= PrivilegeMode.M
      val withSupervisorPrivilege = privilege >= PrivilegeMode.S
    }
  }
}
