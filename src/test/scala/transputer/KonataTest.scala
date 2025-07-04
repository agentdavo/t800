package transputer

import org.scalatest.funsuite.AnyFunSuite
import transputer.test.konata._
import java.io.File

class KonataTest extends AnyFunSuite {
  
  test("Generate example Konata log") {
    val konata = new KonataBackend(new File("generated/example.kanata"))
    
    // Simulate a simple T9000 instruction sequence: 
    // ldc 10; ldc 20; add
    
    var cycle = 0L
    
    // Instruction 0: ldc 10
    val id0 = konata.startInstruction(cycle, 0x80000000L, "ldc 10")
    konata.startStage(cycle, id0, T9000Konata.Stage.FETCH)
    
    cycle += 1
    konata.endStage(cycle, id0, T9000Konata.Stage.FETCH)
    konata.startStage(cycle, id0, T9000Konata.Stage.DECODE)
    konata.addComment(cycle, id0, "Load constant 10 to Areg")
    
    // Instruction 1: ldc 20 (starts fetching)
    val id1 = konata.startInstruction(cycle, 0x80000001L, "ldc 20")
    konata.startStage(cycle, id1, T9000Konata.Stage.FETCH)
    
    cycle += 1
    konata.endStage(cycle, id0, T9000Konata.Stage.DECODE)
    konata.startStage(cycle, id0, T9000Konata.Stage.ADDRESS)
    konata.endStage(cycle, id1, T9000Konata.Stage.FETCH)
    konata.startStage(cycle, id1, T9000Konata.Stage.DECODE)
    konata.addComment(cycle, id1, "Load constant 20, push 10 to Breg")
    
    // Instruction 2: add (starts fetching)
    val id2 = konata.startInstruction(cycle, 0x80000002L, "add")
    konata.startStage(cycle, id2, T9000Konata.Stage.FETCH)
    
    cycle += 1
    konata.endStage(cycle, id0, T9000Konata.Stage.ADDRESS)
    konata.startStage(cycle, id0, T9000Konata.Stage.EXECUTE)
    konata.endStage(cycle, id1, T9000Konata.Stage.DECODE)
    konata.startStage(cycle, id1, T9000Konata.Stage.ADDRESS)
    konata.endStage(cycle, id2, T9000Konata.Stage.FETCH)
    konata.startStage(cycle, id2, T9000Konata.Stage.DECODE)
    konata.addComment(cycle, id2, "Add Areg + Breg")
    
    cycle += 1
    konata.endStage(cycle, id0, T9000Konata.Stage.EXECUTE)
    konata.startStage(cycle, id0, T9000Konata.Stage.WRITEBACK)
    konata.endStage(cycle, id1, T9000Konata.Stage.ADDRESS)
    konata.startStage(cycle, id1, T9000Konata.Stage.EXECUTE)
    konata.endStage(cycle, id2, T9000Konata.Stage.DECODE)
    konata.startStage(cycle, id2, T9000Konata.Stage.ADDRESS)
    
    cycle += 1
    konata.endStage(cycle, id0, T9000Konata.Stage.WRITEBACK)
    konata.retire(cycle, id0)
    konata.endStage(cycle, id1, T9000Konata.Stage.EXECUTE)
    konata.startStage(cycle, id1, T9000Konata.Stage.WRITEBACK)
    konata.endStage(cycle, id2, T9000Konata.Stage.ADDRESS)
    konata.startStage(cycle, id2, T9000Konata.Stage.EXECUTE)
    
    // Add dependency: add depends on both ldc instructions
    konata.addDependency(cycle, id2, id0)
    konata.addDependency(cycle, id2, id1)
    
    cycle += 1
    konata.endStage(cycle, id1, T9000Konata.Stage.WRITEBACK)
    konata.retire(cycle, id1)
    konata.endStage(cycle, id2, T9000Konata.Stage.EXECUTE)
    konata.startStage(cycle, id2, T9000Konata.Stage.WRITEBACK)
    konata.addComment(cycle, id2, "Result: 30 in Areg")
    
    cycle += 1
    konata.endStage(cycle, id2, T9000Konata.Stage.WRITEBACK)
    konata.retire(cycle, id2)
    
    // Example of pipeline stall (lane 1)
    cycle += 1
    val id3 = konata.startInstruction(cycle, 0x80000003L, "ldl 0")
    konata.startStage(cycle, id3, T9000Konata.Stage.FETCH)
    konata.addComment(cycle, id3, "Load from local workspace")
    
    cycle += 1
    konata.endStage(cycle, id3, T9000Konata.Stage.FETCH)
    konata.startStage(cycle, id3, T9000Konata.Stage.DECODE)
    
    // Simulate cache miss - stall for 2 cycles
    konata.startStage(cycle, id3, T9000Konata.Stage.ADDRESS, lane = 1) // Stall lane
    konata.addComment(cycle, id3, "Cache miss - stalling", mouseOver = true)
    
    cycle += 2 // 2 cycle stall
    konata.endStage(cycle, id3, T9000Konata.Stage.ADDRESS, lane = 1)
    
    konata.endStage(cycle, id3, T9000Konata.Stage.DECODE)
    konata.startStage(cycle, id3, T9000Konata.Stage.ADDRESS)
    
    cycle += 1
    konata.endStage(cycle, id3, T9000Konata.Stage.ADDRESS)
    konata.startStage(cycle, id3, T9000Konata.Stage.EXECUTE)
    
    cycle += 1
    konata.endStage(cycle, id3, T9000Konata.Stage.EXECUTE)
    konata.startStage(cycle, id3, T9000Konata.Stage.WRITEBACK)
    
    cycle += 1
    konata.endStage(cycle, id3, T9000Konata.Stage.WRITEBACK)
    konata.retire(cycle, id3)
    
    // Flush example - branch misprediction
    cycle += 1
    val id4 = konata.startInstruction(cycle, 0x80000004L, "cj fail")
    konata.startStage(cycle, id4, T9000Konata.Stage.FETCH)
    
    val id5 = konata.startInstruction(cycle, 0x80000005L, "ldc 99")
    konata.startStage(cycle, id5, T9000Konata.Stage.FETCH)
    
    cycle += 1
    konata.endStage(cycle, id4, T9000Konata.Stage.FETCH)
    konata.startStage(cycle, id4, T9000Konata.Stage.DECODE)
    konata.endStage(cycle, id5, T9000Konata.Stage.FETCH)
    konata.startStage(cycle, id5, T9000Konata.Stage.DECODE)
    
    cycle += 1
    konata.endStage(cycle, id4, T9000Konata.Stage.DECODE)
    konata.startStage(cycle, id4, T9000Konata.Stage.ADDRESS)
    konata.addComment(cycle, id4, "Branch taken - flush pipeline")
    
    // Flush the speculatively fetched instruction
    konata.retire(cycle, id5, flush = true)
    
    cycle += 1
    konata.endStage(cycle, id4, T9000Konata.Stage.ADDRESS)
    konata.startStage(cycle, id4, T9000Konata.Stage.EXECUTE)
    
    cycle += 1
    konata.endStage(cycle, id4, T9000Konata.Stage.EXECUTE)
    konata.startStage(cycle, id4, T9000Konata.Stage.WRITEBACK)
    
    cycle += 1
    konata.endStage(cycle, id4, T9000Konata.Stage.WRITEBACK)
    konata.retire(cycle, id4)
    
    // Close the log
    konata.flush(cycle)
    konata.close()
    
    println("Generated Konata log: generated/example.kanata")
    println("View with: https://github.com/shioyadan/Konata")
  }
  
  test("Decode T9000 mnemonics") {
    // Test primary instructions
    assert(T9000Konata.decodeMnemonic(0x44, 10) == "ldc 10")
    assert(T9000Konata.decodeMnemonic(0x94, 0x100) == "call 100")
    assert(T9000Konata.decodeMnemonic(0xD4, 5) == "stl 5")
    
    // Test secondary instructions
    assert(T9000Konata.decodeMnemonic(0xF5, 0) == "add")
    assert(T9000Konata.decodeMnemonic(0xF0, 0) == "rev")
    assert(T9000Konata.decodeMnemonic(0xF6, 0) == "dup")
    
    // Test with pfix
    assert(T9000Konata.decodeMnemonic(0x24, 0x10) == "pfix 10")
  }
}