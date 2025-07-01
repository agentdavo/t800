package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.schedule.{SchedService, ProcessState}

/** Comprehensive tests for T9000 process scheduler.
  *
  * Tests the T9000 scheduler implementation:
  *   - Dual priority queue management (high/low priority)
  *   - Process state transitions (RUNNING, READY, WAITING, TERMINATED)
  *   - Priority-based preemption
  *   - Process enqueueing and dequeueing
  *   - Queue persistence for SAVEH/SAVEL operations
  *   - Process lifecycle management
  */
class T9000SchedulerSpec extends AnyFunSuite {

  // Simple DUT to test scheduler operations in isolation
  class SchedulerTestDut extends Component {
    val db = Transputer.defaultDatabase()
    db(Global.OPCODE_BITS) = 8

    val io = new Bundle {
      val enqueuePtr = in UInt (32 bits)
      val enqueueHigh = in Bool ()
      val enqueueEnable = in Bool ()
      val terminateEnable = in Bool ()
      val yieldEnable = in Bool ()
      val saveHighEnable = in Bool ()
      val saveLowEnable = in Bool ()

      val currentProc = out UInt (32 bits)
      val nextProc = out UInt (32 bits)
      val hasReady = out Bool ()
      val hasHighPriority = out Bool ()
      val hasLowPriority = out Bool ()
      val processCount = out UInt (16 bits)
      val hiFront = out UInt (32 bits)
      val hiBack = out UInt (32 bits)
      val loFront = out UInt (32 bits)
      val loBack = out UInt (32 bits)
    }

    // Create test system with Scheduler plugin
    val testPlugins = Seq(
      new transputer.plugins.transputer.TransputerPlugin(),
      new transputer.plugins.schedule.SchedulerPlugin()
    )
    val core = Database(db).on(Transputer(testPlugins))
    val schedService = core.host[transputer.plugins.schedule.SchedService]

    // Scheduler control logic
    when(io.enqueueEnable) {
      schedService.enqueue(io.enqueuePtr, io.enqueueHigh)
    }

    when(io.terminateEnable) {
      schedService.terminateCurrent()
    }

    when(io.yieldEnable) {
      schedService.yieldCurrent()
    }

    when(io.saveHighEnable) {
      schedService.saveHighQueue()
    }

    when(io.saveLowEnable) {
      schedService.saveLowQueue()
    }

    // Output current scheduler state
    io.currentProc := schedService.currentProc
    io.nextProc := schedService.nextProc
    io.hasReady := schedService.hasReady
    io.hasHighPriority := schedService.hasHighPriority
    io.hasLowPriority := schedService.hasLowPriority
    io.processCount := schedService.processCount()
    io.hiFront := schedService.hiFront
    io.hiBack := schedService.hiBack
    io.loFront := schedService.loFront
    io.loBack := schedService.loBack
  }

  test("T9000 scheduler dual priority queue operations") {
    SimConfig.withWave.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Initially no processes should be ready
      dut.clockDomain.waitSampling()
      assert(!dut.io.hasReady.toBoolean, "Scheduler should have no ready processes initially")
      assert(!dut.io.hasHighPriority.toBoolean, "High priority queue should be empty initially")
      assert(!dut.io.hasLowPriority.toBoolean, "Low priority queue should be empty initially")
      assert(dut.io.processCount.toLong == 0, "Process count should be 0 initially")

      // Enqueue a low priority process
      dut.io.enqueuePtr #= 0x1000
      dut.io.enqueueHigh #= false
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      assert(dut.io.hasReady.toBoolean, "Scheduler should have ready processes")
      assert(!dut.io.hasHighPriority.toBoolean, "High priority queue should still be empty")
      assert(dut.io.hasLowPriority.toBoolean, "Low priority queue should have process")
      assert(dut.io.processCount.toLong == 1, "Process count should be 1")

      // Enqueue a high priority process
      dut.io.enqueuePtr #= 0x2000
      dut.io.enqueueHigh #= true
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      assert(dut.io.hasHighPriority.toBoolean, "High priority queue should have process")
      assert(dut.io.hasLowPriority.toBoolean, "Low priority queue should still have process")
      assert(dut.io.processCount.toLong == 2, "Process count should be 2")
    }
  }

  test("T9000 scheduler priority-based scheduling") {
    SimConfig.withWave.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Enqueue both high and low priority processes
      dut.io.enqueuePtr #= 0x1000
      dut.io.enqueueHigh #= false
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      dut.io.enqueuePtr #= 0x2000
      dut.io.enqueueHigh #= true
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      // Trigger scheduling by terminating current process
      dut.io.terminateEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.terminateEnable #= false
      dut.clockDomain.waitSampling(3)

      // High priority process should be scheduled first
      assert(
        dut.io.nextProc.toLong == 0x2000,
        s"High priority process should be scheduled first, got 0x${dut.io.nextProc.toLong.toHexString}"
      )

      // After high priority completes, low priority should be next
      dut.io.terminateEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.terminateEnable #= false
      dut.clockDomain.waitSampling(3)

      assert(
        dut.io.nextProc.toLong == 0x1000,
        s"Low priority process should be scheduled next, got 0x${dut.io.nextProc.toLong.toHexString}"
      )
    }
  }

  test("T9000 scheduler process termination") {
    SimConfig.withWave.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enqueue multiple processes
      for (i <- 0 until 3) {
        dut.io.enqueuePtr #= 0x1000 + (i * 0x100)
        dut.io.enqueueHigh #= false
        dut.io.enqueueEnable #= true
        dut.clockDomain.waitSampling()
        dut.io.enqueueEnable #= false
        dut.clockDomain.waitSampling()
      }

      val initialCount = dut.io.processCount.toLong
      assert(initialCount == 3, "Should have 3 processes enqueued")

      // Terminate processes one by one
      for (i <- 0 until 3) {
        dut.io.terminateEnable #= true
        dut.clockDomain.waitSampling()
        dut.io.terminateEnable #= false
        dut.clockDomain.waitSampling(2)

        val expectedCount = initialCount - (i + 1)
        assert(
          dut.io.processCount.toLong == expectedCount,
          s"Process count should be $expectedCount after ${i + 1} terminations"
        )
      }
    }
  }

  test("T9000 scheduler yield operation") {
    SimConfig.withWave.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enqueue two processes
      dut.io.enqueuePtr #= 0x1000
      dut.io.enqueueHigh #= false
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      dut.io.enqueuePtr #= 0x2000
      dut.io.enqueueHigh #= false
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      // Start first process
      dut.io.terminateEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.terminateEnable #= false
      dut.clockDomain.waitSampling(2)

      val firstProc = dut.io.nextProc.toLong

      // Yield current process (STOPP equivalent)
      dut.io.yieldEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.yieldEnable #= false
      dut.clockDomain.waitSampling(3)

      // Should switch to second process
      val secondProc = dut.io.nextProc.toLong
      assert(secondProc != firstProc, "Yield should cause context switch to different process")
    }
  }

  test("T9000 scheduler queue state persistence") {
    SimConfig.withWave.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enqueue processes to both queues
      dut.io.enqueuePtr #= 0x1000
      dut.io.enqueueHigh #= true
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      dut.io.enqueuePtr #= 0x2000
      dut.io.enqueueHigh #= false
      dut.io.enqueueEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.enqueueEnable #= false
      dut.clockDomain.waitSampling()

      // Save queue states (SAVEH/SAVEL equivalent)
      val initialHiFront = dut.io.hiFront.toLong
      val initialHiBack = dut.io.hiBack.toLong
      val initialLoFront = dut.io.loFront.toLong
      val initialLoBack = dut.io.loBack.toLong

      dut.io.saveHighEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.saveHighEnable #= false

      dut.io.saveLowEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.saveLowEnable #= false
      dut.clockDomain.waitSampling()

      // Verify queue counters increment as expected
      assert(dut.io.hiBack.toLong > initialHiFront, "High priority back should be ahead of front")
      assert(dut.io.loBack.toLong > initialLoFront, "Low priority back should be ahead of front")
    }
  }

  test("T9000 scheduler process count tracking") {
    SimConfig.withWave.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      assert(dut.io.processCount.toLong == 0, "Initial process count should be 0")

      // Add processes and verify count increases
      for (i <- 1 to 5) {
        dut.io.enqueuePtr #= 0x1000 + (i * 0x100)
        dut.io.enqueueHigh #= (i % 2 == 0) // Alternate priority
        dut.io.enqueueEnable #= true
        dut.clockDomain.waitSampling()
        dut.io.enqueueEnable #= false
        dut.clockDomain.waitSampling()

        assert(dut.io.processCount.toLong == i, s"Process count should be $i")
      }

      // Remove processes and verify count decreases
      for (i <- 5 to 1 by -1) {
        dut.io.terminateEnable #= true
        dut.clockDomain.waitSampling()
        dut.io.terminateEnable #= false
        dut.clockDomain.waitSampling(2)

        val expectedCount = i - 1
        assert(
          dut.io.processCount.toLong == expectedCount,
          s"Process count should be $expectedCount after termination"
        )
      }
    }
  }
}
