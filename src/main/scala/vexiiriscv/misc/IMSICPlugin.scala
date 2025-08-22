package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.{CsrAccessPlugin, CsrCondFilter, CsrListFilter, CsrRamPlugin, CsrRamService}
import vexiiriscv.Global._
import vexiiriscv.riscv.{IndirectCSR, _}
import vexiiriscv.riscv.Riscv._
import spinal.core
import scala.collection.mutable.ArrayBuffer

// sourceIds?
case class IMSICPlugin(val sourceIds : Seq[Int], val withSupervisor : Boolean) extends FiberPlugin {
  case class Request(idWidth: Int) extends Bundle {
    val id  = UInt(idWidth bits)
    val iep = Bool()
  }

  val idWidth = log2Up((sourceIds ++ Seq(0)).max + 1)

  val logic = during setup new Area {
    val indirect = host[IndirectCsrPlugin]
    val cap = host[CsrAccessPlugin]
    val buildBefore = retains(cap.csrLock)

    val topiId = CODE().assignDontCare()

    awaitBuild()

    val harts = for (hartId <- 0 until HART_COUNT) yield new Area {
      val hart = indirect.logic.harts(hartId)
      val api = hart.api
      val m = new Area {
        val eidelivery = out(RegInit(U(0x40000000, XLEN bits)))
        val eithreshold = RegInit(U(0, XLEN bits))

        val deliveryFilter = hart.m.getCsrFilter(IndirectCSR.eidelivery, CSR.MIREG)
        val thresholdFilter = hart.m.getCsrFilter(IndirectCSR.eithreshold, CSR.MIREG)

        api.read(eidelivery, deliveryFilter)
        api.write(eidelivery, deliveryFilter)
        api.read(eithreshold, thresholdFilter)
        api.write(eithreshold, thresholdFilter)

        // tmp
        for (i <- 0 until 16) {
          val iprioFilter = hart.m.getCsrFilter(IndirectCSR.iprio0 + i, CSR.MIREG)
          api.read(U(0), iprioFilter)
        }

        val sources = for ((sourceId, i) <- sourceIds.zipWithIndex) yield new Area {
          val id = sourceId
          val ie = RegInit(False)
          val ip = RegInit(False)
          val trigger = in Bool()
          val offset = sourceId / XLEN * (1 + (XLEN == 64).toInt)

          ip.setWhen(trigger)

          val eieFilter = hart.m.getCsrFilter(IndirectCSR.eie0 + offset, CSR.MIREG)
          val eipFilter = hart.m.getCsrFilter(IndirectCSR.eip0 + offset, CSR.MIREG)

          api.read(ie, eieFilter, sourceId % XLEN)
          api.write(ie, eieFilter, sourceId % XLEN)
          api.read(ip, eipFilter, sourceId % XLEN)
          api.write(ip, eipFilter, sourceId % XLEN)
        }

        val requests = sources.map { s =>
          val r = Request(idWidth)
          r.id  := s.id
          r.iep := s.ie && s.ip
          r
        }

        val result = RegNext(requests.reduceBalancedTree((a, b) => {
          val takeA = !b.iep || (a.iep && a.id < b.id)
          takeA ? a | b
        }))

        val identity = out((result.iep && (eithreshold === 0 || result.id < eithreshold)) ? result.id | 0)

        api.read(CSR.MTOPEI, 0 -> identity, 16 -> identity)
        // onlyonfire?
        api.onWrite(CSR.MTOPEI, false) {
          switch(identity) {
            for (source <- sources) {
              is (source.id) {
                source.ip.clear()
              }
            }
          }
        }

        api.read(CSR.MTOPI, 0 -> U(1), 16 -> topiId)

        def eipArbiter(aplicTarget: Bool): Bool = {
          eidelivery.mux(
            1 -> (identity > 0),
            0x40000000 -> aplicTarget,
            default -> False
          )
        }
      }
// change to define
      val s = withSupervisor generate new Area {
        val eidelivery = out(RegInit(U(0x40000000, XLEN bits)))
        val eithreshold = RegInit(U(0, XLEN bits))

        val deliveryFilter = hart.s.getCsrFilter(IndirectCSR.eidelivery, CSR.SIREG)
        val thresholdFilter = hart.s.getCsrFilter(IndirectCSR.eithreshold, CSR.SIREG)

        api.read(eidelivery, deliveryFilter)
        api.write(eidelivery, deliveryFilter)
        api.read(eithreshold, thresholdFilter)
        api.write(eithreshold, thresholdFilter)

        // tmp
        for (i <- 0 until 16) {
          val iprioFilter = hart.s.getCsrFilter(IndirectCSR.iprio0 + i, CSR.SIREG)
          api.read(U(0), iprioFilter)
        }

        val sources = for ((sourceId, i) <- sourceIds.zipWithIndex) yield new Area {
          val id = sourceId
          val ie = RegInit(False)
          val ip = RegInit(False)
          val trigger = in Bool()
          val offset = sourceId / XLEN * (1 + (XLEN == 64).toInt)

          ip.setWhen(trigger)

          val eieFilter = hart.s.getCsrFilter(IndirectCSR.eie0 + offset, CSR.SIREG)
          val eipFilter = hart.s.getCsrFilter(IndirectCSR.eip0 + offset, CSR.SIREG)

          api.read(ie, eieFilter, sourceId % XLEN)
          api.write(ie, eieFilter, sourceId % XLEN)
          api.read(ip, eipFilter, sourceId % XLEN)
          api.write(ip, eipFilter, sourceId % XLEN)
        }

        val requests = sources.map { s =>
          val r = Request(idWidth)
          r.id  := s.id
          r.iep := s.ie && s.ip
          r
        }

        val result = RegNext(requests.reduceBalancedTree((a, b) => {
          val takeA = !b.iep || (a.iep && a.id < b.id)
          takeA ? a | b
        }))

        val identity = out((result.iep && (eithreshold === 0 || result.id < eithreshold)) ? result.id | 0)

        api.read(CSR.STOPEI, 0 -> identity, 16 -> identity)
        // onlyonfire?
        api.onWrite(CSR.STOPEI, false) {
          switch(identity) {
            for (source <- sources) {
              is (source.id) {
                source.ip.clear()
              }
            }
          }
        }

        api.read(CSR.STOPI, 0 -> U(1), 16 -> topiId)

        def eipArbiter(aplicTarget: Bool): Bool = {
          eidelivery.mux(
            1 -> (identity > 0),
            0x40000000 -> aplicTarget,
            default -> False
          )
        }
      }
    }
    buildBefore.release()
  }
}
