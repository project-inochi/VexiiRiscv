package vexiiriscv.misc

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline.Link

/*
Here is the linux DTS which can be used to interface the VexiiRiscv PMU :

        pmu {
		      compatible 			= "riscv,pmu";
		      riscv,event-to-mhpmevent =
 					 <0x1 0x0000 0x06>, /*  Cycle */
					 <0x2 0x0000 0x07>, /*  instructions */
 					 <0x5 0x0000 0x01>, /*  Conditional branch instruction count */
					 <0x6 0x0000 0x02>, /*  Misprediction of conditional branches */
 					 <0x8 0x0000 0x04>, /*  STALLED_CYCLES_FRONTEND */
 					 <0x9 0x0000 0x05>, /*  STALLED_CYCLES_BACKEND */
					 <0x10000 0x0000 0x18>, /*  D-Cache load access */
					 <0x10001 0x0000 0x19>, /*  D-Cache load miss */
					 <0x10008 0x0000 0x10>,  /* I-Cache access */
					 <0x10009 0x0000 0x11>;  /* I-Cache miss */

	        riscv,event-to-mhpmcounters =
            <0x00001 0x00009 0xFF8>,
            <0x10000 0x10009 0xFF8>;

		      riscv,raw-event-to-mhpmcounters = <0x0000 0x0000 0xffffffff 0xffffff00 0x00000ff8>;
        };

Also, don't forget to enable the PMU support in the linux defconfig.
*/

object PerformanceCounterService{
  /** Version of the portable raw-event namespace used by SHDLT firmware. */
  val EVENT_SCHEMA_VERSION = 1

  val BRANCH_COUNT   = 0x01
  val BRANCH_MISS    = 0x02

  val STALLED_CYCLES_FRONTEND = 0x04 // => 8
  val STALLED_CYCLES_BACKEND = 0x05 // => 9

  val CYCLES = 0x06
  val INSTRUCTIONS = 0x07

  val ICACHE_ACCESS     = 0x10
  val ICACHE_MISS       = 0x11
  val ICACHE_WAITING    = 0x12
  val ICACHE_TLB_CYCLES = 0x13

  val DCACHE_LOAD_ACCESS = 0x18
  val DCACHE_LOAD_MISS   = 0x19
  val DCACHE_WAITING     = 0x1A
  val DCACHE_TLB_CYCLES  = 0x1B


  val DEV = 0x20

  // SHDLT architectural events.  Keep these IDs stable: software may select
  // them through mhpmevent without knowing the internal cache/MMU topology.
  val SHDLT_D_TRANSITION          = 0x30 // PTE.D transitioned 0 -> 1
  val SHDLT_LOG_APPEND            = 0x31 // committed dirty-log entry
  val SHDLT_PTE_CAS_ATTEMPT       = 0x32 // accepted PTE CAS command
  val SHDLT_PTE_CAS_RETRY         = 0x33 // CAS response requested retry
  val SHDLT_LOG_FAULT             = 0x34 // architecturally reported log fault
  val SHDLT_HFENCE_GVMA           = 0x35 // accepted HFENCE.GVMA
  // The following implementation-diagnostic events are intentionally kept
  // separate from the architectural list below.
  val SHDLT_SHADOW_TLB_REFILL     = 0x36 // completed G-stage refill
  val SHDLT_SHADOW_TLB_INVALIDATE = 0x37 // completed G-stage invalidation
  val LSU_STORE_ACCESS            = 0x38 // accepted non-prefetch store
  val LSU_STORE_MISS              = 0x39 // store requiring L1 refill
  val LSU_COHERENCE_RETRY         = 0x3A // coherence response requested retry
  val MMU_TLB_REFILL              = 0x3B // completed first-stage refill
  val MMU_TLB_MISS                = 0x3C // first-stage miss accepted
  val SHDLT_UPDATE_BUSY_CYCLES    = 0x3D // cycles in PTE update service
  val SHDLT_LOG_BUSY_CYCLES       = 0x3E // cycles in log service
  val SHDLT_CAS_BUSY_CYCLES       = 0x3F // cycles in CAS service

  /** Canonical names for reports, DTS generators, and firmware metadata. */
  val EVENT_NAMES: Map[Int, String] = Map(
    BRANCH_COUNT -> "branch_count", BRANCH_MISS -> "branch_miss",
    STALLED_CYCLES_FRONTEND -> "stalled_cycles_frontend",
    STALLED_CYCLES_BACKEND -> "stalled_cycles_backend",
    CYCLES -> "cycles", INSTRUCTIONS -> "instructions",
    ICACHE_ACCESS -> "icache_access", ICACHE_MISS -> "icache_miss",
    ICACHE_WAITING -> "icache_waiting", ICACHE_TLB_CYCLES -> "icache_tlb_cycles",
    DCACHE_LOAD_ACCESS -> "dcache_load_access", DCACHE_LOAD_MISS -> "dcache_load_miss",
    DCACHE_WAITING -> "dcache_waiting", DCACHE_TLB_CYCLES -> "dcache_tlb_cycles",
    SHDLT_D_TRANSITION -> "shdlt_d_transition",
    SHDLT_LOG_APPEND -> "shdlt_log_append",
    SHDLT_PTE_CAS_ATTEMPT -> "shdlt_pte_cas_attempt",
    SHDLT_PTE_CAS_RETRY -> "shdlt_pte_cas_retry",
    SHDLT_LOG_FAULT -> "shdlt_log_fault",
    SHDLT_HFENCE_GVMA -> "shdlt_hfence_gvma",
    SHDLT_SHADOW_TLB_REFILL -> "shdlt_shadow_tlb_refill",
    SHDLT_SHADOW_TLB_INVALIDATE -> "shdlt_shadow_tlb_invalidate",
    LSU_STORE_ACCESS -> "lsu_store_access", LSU_STORE_MISS -> "lsu_store_miss",
    LSU_COHERENCE_RETRY -> "lsu_coherence_retry",
    MMU_TLB_REFILL -> "mmu_tlb_refill", MMU_TLB_MISS -> "mmu_tlb_miss",
    SHDLT_UPDATE_BUSY_CYCLES -> "shdlt_update_busy_cycles",
    SHDLT_LOG_BUSY_CYCLES -> "shdlt_log_busy_cycles",
    SHDLT_CAS_BUSY_CYCLES -> "shdlt_cas_busy_cycles"
  )

  /** Events whose meaning is defined at an architectural acceptance/commit
    * boundary and can therefore be consumed by every backend. */
  val SHDLT_ARCHITECTURAL_EVENTS: Seq[Int] = Seq(
    SHDLT_D_TRANSITION, SHDLT_LOG_APPEND, SHDLT_PTE_CAS_ATTEMPT,
    SHDLT_PTE_CAS_RETRY, SHDLT_LOG_FAULT, SHDLT_HFENCE_GVMA
  )

  /** Optional implementation diagnostics.  These counters are useful for
    * tuning a particular cache/MMU, but are never required for a portable
    * correctness result and may be unavailable on hardware or Linux. */
  val SHDLT_DIAGNOSTIC_EVENTS: Seq[Int] = Seq(
    SHDLT_SHADOW_TLB_REFILL, SHDLT_SHADOW_TLB_INVALIDATE,
    LSU_STORE_ACCESS, LSU_STORE_MISS, LSU_COHERENCE_RETRY,
    MMU_TLB_REFILL, MMU_TLB_MISS, SHDLT_UPDATE_BUSY_CYCLES,
    SHDLT_LOG_BUSY_CYCLES, SHDLT_CAS_BUSY_CYCLES
  )

  /** Complete set exposed by this implementation, in stable numeric order. */
  val SHDLT_HPM_EVENTS: Seq[Int] =
    SHDLT_ARCHITECTURAL_EVENTS ++ SHDLT_DIAGNOSTIC_EVENTS

  def eventName(id: Int): String = EVENT_NAMES.getOrElse(id, f"raw_0x$id%02x")
}

/**
 * This service allows other plugins to generate new event sources for the PMU
 */
trait PerformanceCounterService {
  def createEventPort(id: Int): Bool
  def createEventPort(id: Int, drive : Bool): Bool = {
    val ret = createEventPort(id)
    ret := drive
    ret
  }
  val elaborationLock = Retainer()
}

trait CommitService {
  def getCommitMask(hartId: Int): Bits
}

trait InflightService {
  def hasInflight(hartId: Int): Bool
}

trait PipelineService{
  def getLinks() : Seq[Link]
}
