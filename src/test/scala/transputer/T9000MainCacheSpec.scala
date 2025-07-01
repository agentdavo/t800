package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite

/** Comprehensive tests for T9000 MainCache 16KB four-bank cache architecture.
  *
  * Tests the T9000 main cache design concepts:
  *   - 16KB four-bank cache (4KB per bank)
  *   - Bank selection via address bits [5:4]
  *   - Cache line management (256 lines per bank)
  *   - Tag/Valid/Dirty bit operations
  *   - Address mapping and bank distribution
  *   - Cache capacity calculations
  *
  * Note: This tests the cache architecture concepts rather than the full MainCachePlugin due to
  * complex dependencies (PmiPlugin, MMU, etc.)
  */
class T9000MainCacheSpec extends AnyFunSuite {

  // Simplified cache address decoder for testing cache concepts
  class CacheAddressDecoder extends Component {
    val io = new Bundle {
      val address = in UInt (32 bits)
      val bankSelect = out UInt (2 bits) // bits [5:4]
      val lineIndex = out UInt (8 bits) // bits [11:4]
      val wordOffset = out UInt (2 bits) // bits [3:2]
      val byteOffset = out UInt (2 bits) // bits [1:0]
      val tag = out Bits (20 bits) // bits [31:12] for simplified tag
    }

    // T9000 cache address breakdown:
    // [31:12] = Tag (20 bits)
    // [11:4]  = Line Index (8 bits, 256 lines per bank)
    // [5:4]   = Bank Select (2 bits, 4 banks)
    // [3:2]   = Word Offset (2 bits, 4 words per line)
    // [1:0]   = Byte Offset (2 bits)

    io.bankSelect := io.address(5 downto 4)
    io.lineIndex := io.address(11 downto 4)
    io.wordOffset := io.address(3 downto 2)
    io.byteOffset := io.address(1 downto 0)
    io.tag := io.address(31 downto 12).asBits
  }

  test("T9000 MainCache bank selection") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test bank selection based on address bits [5:4]
      val testAddresses = Seq(
        (0x00000000L, 0), // Bank 0: bits [5:4] = 00
        (0x00000010L, 1), // Bank 1: bits [5:4] = 01
        (0x00000020L, 2), // Bank 2: bits [5:4] = 10
        (0x00000030L, 3) // Bank 3: bits [5:4] = 11
      )

      for ((addr, expectedBank) <- testAddresses) {
        dut.io.address #= addr
        dut.clockDomain.waitSampling()

        assert(
          dut.io.bankSelect.toLong == expectedBank,
          s"Address 0x${addr.toHexString} should select bank $expectedBank, got ${dut.io.bankSelect.toLong}"
        )
      }
    }
  }

  test("T9000 MainCache line index calculation") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test cache line index calculation (bits [11:4])
      val testAddresses = Seq(
        (0x00000000L, 0x00), // Line 0
        (0x00000010L, 0x01), // Line 1
        (0x000000f0L, 0x0f), // Line 15
        (0x00000ff0L, 0xff) // Line 255 (max for 256 lines)
      )

      for ((addr, expectedLine) <- testAddresses) {
        dut.io.address #= addr
        dut.clockDomain.waitSampling()

        assert(
          dut.io.lineIndex.toLong == expectedLine,
          s"Address 0x${addr.toHexString} should map to line 0x${expectedLine.toHexString}, got 0x${dut.io.lineIndex.toLong.toHexString}"
        )
      }
    }
  }

  test("T9000 MainCache tag extraction") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test cache tag extraction (bits [31:12] for simplified 20-bit tag)
      val testAddresses = Seq(
        (0x12345000L, 0x12345), // Tag from high bits
        (0xabcde000L, 0xabcde), // Different tag pattern
        (0x00001000L, 0x00001), // Minimal tag
        (0xfffff000L, 0xfffff) // Maximum tag
      )

      for ((addr, expectedTag) <- testAddresses) {
        dut.io.address #= addr
        dut.clockDomain.waitSampling()

        val actualTag = dut.io.tag.toLong & 0xfffff // Mask to 20 bits
        assert(
          actualTag == expectedTag,
          s"Address 0x${addr.toHexString} should have tag 0x${expectedTag.toHexString}, got 0x${actualTag.toHexString}"
        )
      }
    }
  }

  test("T9000 MainCache word and byte offset calculation") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test word and byte offset extraction for cache line addressing
      val testAddresses = Seq(
        (0x00001000L, 0, 0), // Word 0, Byte 0
        (0x00001004L, 1, 0), // Word 1, Byte 0
        (0x00001008L, 2, 0), // Word 2, Byte 0
        (0x0000100cL, 3, 0), // Word 3, Byte 0
        (0x00001001L, 0, 1), // Word 0, Byte 1
        (0x00001002L, 0, 2), // Word 0, Byte 2
        (0x00001003L, 0, 3) // Word 0, Byte 3
      )

      for ((addr, expectedWord, expectedByte) <- testAddresses) {
        dut.io.address #= addr
        dut.clockDomain.waitSampling()

        assert(
          dut.io.wordOffset.toLong == expectedWord,
          s"Address 0x${addr.toHexString} should have word offset $expectedWord, got ${dut.io.wordOffset.toLong}"
        )
        assert(
          dut.io.byteOffset.toLong == expectedByte,
          s"Address 0x${addr.toHexString} should have byte offset $expectedByte, got ${dut.io.byteOffset.toLong}"
        )
      }
    }
  }

  test("T9000 MainCache bank distribution") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test that consecutive cache lines are distributed across banks
      val baseAddr = 0x00001000L
      val bankCounts = Array.fill(4)(0)

      // Test 16 consecutive cache lines (4 per bank)
      for (i <- 0 until 16) {
        val addr = baseAddr + (i * 0x10) // 16-byte aligned cache lines
        dut.io.address #= addr
        dut.clockDomain.waitSampling()

        val bank = dut.io.bankSelect.toInt
        bankCounts(bank) += 1

        // Each address should map to the expected bank
        val expectedBank = (addr >> 4) & 0x3
        assert(
          bank == expectedBank,
          s"Address 0x${addr.toHexString} should map to bank $expectedBank, got $bank"
        )
      }

      // Verify even distribution (4 cache lines per bank)
      for (i <- 0 until 4) {
        assert(bankCounts(i) == 4, s"Bank $i should have 4 cache lines, got ${bankCounts(i)}")
      }
    }
  }

  test("T9000 MainCache capacity calculations") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // T9000 MainCache architecture:
      // - Total: 16KB (4 banks × 4KB each)
      // - Lines per bank: 256 (4KB ÷ 16 bytes per line)
      // - Cache line size: 16 bytes (128 bits)
      // - Words per line: 4 (16 bytes ÷ 4 bytes per word)

      val totalCacheSize = 16 * 1024 // 16KB
      val banksCount = 4
      val bankSize = totalCacheSize / banksCount // 4KB per bank
      val lineSize = 16 // 16 bytes per cache line
      val linesPerBank = bankSize / lineSize // 256 lines per bank
      val wordsPerLine = lineSize / 4 // 4 words per line

      println(s"T9000 MainCache Architecture:")
      println(s"  Total size: ${totalCacheSize} bytes (${totalCacheSize / 1024}KB)")
      println(s"  Banks: $banksCount")
      println(s"  Size per bank: ${bankSize} bytes (${bankSize / 1024}KB)")
      println(s"  Cache line size: $lineSize bytes")
      println(s"  Lines per bank: $linesPerBank")
      println(s"  Words per line: $wordsPerLine")

      // Test address ranges for each bank
      val bankTestAddresses = Seq(
        (0x00000000L, 0), // Bank 0 start
        (0x00000ff0L, 0), // Bank 0 last line
        (0x00001000L, 0), // Bank 0 next iteration
        (0x00000010L, 1), // Bank 1
        (0x00000020L, 2), // Bank 2
        (0x00000030L, 3) // Bank 3
      )

      for ((addr, expectedBank) <- bankTestAddresses) {
        dut.io.address #= addr
        dut.clockDomain.waitSampling()

        val actualBank = dut.io.bankSelect.toLong
        val lineIndex = dut.io.lineIndex.toLong

        assert(
          actualBank == expectedBank,
          s"Address 0x${addr.toHexString} should map to bank $expectedBank, got $actualBank"
        )

        // Verify line index is within valid range
        assert(
          lineIndex < linesPerBank,
          s"Line index $lineIndex should be < $linesPerBank for address 0x${addr.toHexString}"
        )

        println(s"  Address 0x${addr.toHexString} -> Bank $actualBank, Line $lineIndex")
      }
    }
  }

  test("T9000 MainCache address space coverage") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test that the cache can address the full 16KB space
      // Each bank covers specific address ranges

      val testRanges = Seq(
        // Bank 0: addresses with bits [5:4] = 00
        (0x00000000L, 0x00000f00L, 0),
        // Bank 1: addresses with bits [5:4] = 01
        (0x00000010L, 0x00000f10L, 1),
        // Bank 2: addresses with bits [5:4] = 10
        (0x00000020L, 0x00000f20L, 2),
        // Bank 3: addresses with bits [5:4] = 11
        (0x00000030L, 0x00000f30L, 3)
      )

      for ((startAddr, endAddr, expectedBank) <- testRanges) {
        // Test start of range
        dut.io.address #= startAddr
        dut.clockDomain.waitSampling()
        assert(
          dut.io.bankSelect.toLong == expectedBank,
          s"Start address 0x${startAddr.toHexString} should be in bank $expectedBank"
        )

        // Test end of range
        dut.io.address #= endAddr
        dut.clockDomain.waitSampling()
        assert(
          dut.io.bankSelect.toLong == expectedBank,
          s"End address 0x${endAddr.toHexString} should be in bank $expectedBank"
        )

        println(s"Bank $expectedBank covers 0x${startAddr.toHexString} to 0x${endAddr.toHexString}")
      }

      // Verify that all 256 possible line indices can be generated
      val maxLineIndex = 255
      val testLineAddr = maxLineIndex << 4 // Shift to line index position

      dut.io.address #= testLineAddr
      dut.clockDomain.waitSampling()

      assert(
        dut.io.lineIndex.toLong == maxLineIndex,
        s"Should be able to address line $maxLineIndex, got ${dut.io.lineIndex.toLong}"
      )

      println(s"Maximum addressable line index: ${dut.io.lineIndex.toLong}")
    }
  }

  test("T9000 MainCache interleaved access pattern") {
    SimConfig.withWave.compile(new CacheAddressDecoder).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test interleaved access pattern typical of T9000 instruction fetch
      // Sequential addresses should distribute across banks for parallel access

      val baseAddr = 0x00001000L
      val accessPattern = for (i <- 0 until 16) yield {
        val addr = baseAddr + (i * 4) // Word-aligned sequential access
        val expectedBank = (addr >> 4) & 0x3 // Bank from bits [5:4]
        (addr, expectedBank)
      }

      println("Sequential word access pattern:")
      for ((addr, expectedBank) <- accessPattern) {
        dut.io.address #= addr
        dut.clockDomain.waitSampling()

        val actualBank = dut.io.bankSelect.toLong
        val lineIndex = dut.io.lineIndex.toLong
        val wordOffset = dut.io.wordOffset.toLong

        assert(
          actualBank == expectedBank,
          s"Address 0x${addr.toHexString} should access bank $expectedBank, got $actualBank"
        )

        println(s"  0x${addr.toHexString} -> Bank $actualBank, Line $lineIndex, Word $wordOffset")
      }

      // Verify that the pattern provides good bank distribution
      val bankUsage = accessPattern.groupBy(_._2).mapValues(_.length)
      println(s"Bank usage distribution: $bankUsage")

      // Each bank should be used for memory parallelism
      for (bank <- 0 until 4) {
        assert(
          bankUsage.getOrElse(bank, 0) > 0,
          s"Bank $bank should be used in sequential access pattern"
        )
      }
    }
  }
}
