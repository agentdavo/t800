package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

/** IServer protocol implementation for T9000 transputer communication.
  *
  * The IServer protocol allows communication between transputer and host system for debugging, file
  * I/O, and console operations. This implementation supports the basic protocol used in the hello
  * world example.
  */
object IServerProtocol {
  // IServer protocol constants
  val REQ_PUTS = 0x0f // Console output request
  val REQ_GETS = 0x10 // Console input request
  val REQ_OPEN = 0x0a // File open request
  val REQ_CLOSE = 0x0b // File close request
  val REQ_READ = 0x0c // File read request
  val REQ_WRITE = 0x0d // File write request
  val REQ_SEEK = 0x0e // File seek request

  val STDOUT_STREAMID = 0x01 // Standard output stream
  val STDIN_STREAMID = 0x00 // Standard input stream

  // Protocol frame structure
  case class IServerFrame() extends Bundle {
    val frameLength = UInt(16 bits) // Frame length (excluding this field)
    val requestType = UInt(8 bits) // Request type (REQ_*)
    val streamId = UInt(32 bits) // Stream identifier
    val dataLength = UInt(16 bits) // Data length
    val data = Bits(8 bits) // Data payload (variable length)
  }
}

/** IServer protocol handler for link communication */
class IServerHandler extends Component {
  val io = new Bundle {
    // Link interface
    val linkIn = slave(Stream(Bits(8 bits)))
    val linkOut = master(Stream(Bits(8 bits)))

    // Host interface
    val consoleOut = master(Stream(Bits(8 bits)))
    val consoleIn = slave(Stream(Bits(8 bits)))
    val fileOp = master(Stream(Bits(32 bits))) // File operations

    // Status
    val active = out Bool ()
    val error = out Bool ()
  }

  import IServerProtocol._

  // Protocol state machine
  val protocolFsm = new StateMachine {
    val frameBuffer = Mem(Bits(8 bits), 1024)
    val frameLength = Reg(UInt(16 bits)) init (0)
    val bytesReceived = Reg(UInt(16 bits)) init (0)
    val requestType = Reg(UInt(8 bits)) init (0)
    val streamId = Reg(UInt(32 bits)) init (0)
    val dataLength = Reg(UInt(16 bits)) init (0)

    val IDLE = new State with EntryPoint
    val RECV_LENGTH = new State
    val RECV_HEADER = new State
    val RECV_DATA = new State
    val PROCESS_FRAME = new State
    val SEND_RESPONSE = new State

    // Default outputs
    io.linkIn.ready := False
    io.linkOut.valid := False
    io.linkOut.payload := 0
    io.consoleOut.valid := False
    io.consoleOut.payload := 0
    io.fileOp.valid := False
    io.fileOp.payload := 0
    io.active := !isActive(IDLE)
    io.error := False

    IDLE.whenIsActive {
      io.linkIn.ready := True
      bytesReceived := 0
      when(io.linkIn.valid) {
        frameLength(7 downto 0) := io.linkIn.payload.asUInt
        goto(RECV_LENGTH)
      }
    }

    RECV_LENGTH.whenIsActive {
      io.linkIn.ready := True
      when(io.linkIn.valid) {
        frameLength(15 downto 8) := io.linkIn.payload.asUInt
        bytesReceived := 0
        goto(RECV_HEADER)
      }
    }

    RECV_HEADER.whenIsActive {
      io.linkIn.ready := True
      when(io.linkIn.valid) {
        frameBuffer.write(bytesReceived.resized, io.linkIn.payload)
        bytesReceived := bytesReceived + 1

        switch(bytesReceived) {
          is(0) { requestType := io.linkIn.payload.asUInt }
          is(1) { streamId(7 downto 0) := io.linkIn.payload.asUInt }
          is(2) { streamId(15 downto 8) := io.linkIn.payload.asUInt }
          is(3) { streamId(23 downto 16) := io.linkIn.payload.asUInt }
          is(4) { streamId(31 downto 24) := io.linkIn.payload.asUInt }
          is(5) { dataLength(7 downto 0) := io.linkIn.payload.asUInt }
          is(6) {
            dataLength(15 downto 8) := io.linkIn.payload.asUInt
            when(dataLength === 0) {
              goto(PROCESS_FRAME)
            } otherwise {
              goto(RECV_DATA)
            }
          }
        }
      }
    }

    RECV_DATA.whenIsActive {
      io.linkIn.ready := True
      when(io.linkIn.valid) {
        frameBuffer.write(bytesReceived.resized, io.linkIn.payload)
        bytesReceived := bytesReceived + 1
        when(bytesReceived >= (dataLength + 7)) {
          goto(PROCESS_FRAME)
        }
      }
    }

    PROCESS_FRAME.whenIsActive {
      switch(requestType) {
        is(REQ_PUTS) {
          // Console output request
          when(streamId === STDOUT_STREAMID) {
            // Send data to console output
            io.consoleOut.valid := True
            io.consoleOut.payload := frameBuffer.readSync(U(7))
            when(io.consoleOut.ready) {
              goto(SEND_RESPONSE)
            }
          } otherwise {
            io.error := True
            goto(IDLE)
          }
        }
        is(REQ_GETS) {
          // Console input request
          when(streamId === STDIN_STREAMID) {
            // Handle console input
            goto(SEND_RESPONSE)
          } otherwise {
            io.error := True
            goto(IDLE)
          }
        }
        default {
          // Unsupported request
          io.error := True
          goto(IDLE)
        }
      }
    }

    SEND_RESPONSE.whenIsActive {
      // Send response frame (simplified)
      io.linkOut.valid := True
      io.linkOut.payload := 0x00 // Success response
      when(io.linkOut.ready) {
        goto(IDLE)
      }
    }
  }
}

/** Test component for IServer protocol */
class IServerTest extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val message = in Bits (8 bits)
    val messageLength = in UInt (8 bits)
    val sent = out Bool ()
  }

  val iserver = new IServerHandler()

  // Simple test logic to send a console message
  val sendFsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val SEND_LENGTH = new State
    val SEND_FRAME = new State
    val DONE = new State

    val frameBytes = Vec(Reg(Bits(8 bits)), 16)
    val byteIndex = Reg(UInt(4 bits)) init (0)
    val frameLength = Reg(UInt(4 bits)) init (0)

    iserver.io.linkIn.valid := False
    iserver.io.linkIn.payload := 0
    io.sent := False

    IDLE.whenIsActive {
      when(io.start) {
        // Prepare frame: length(2) + type(1) + streamId(4) + dataLen(2) + data(n)
        frameLength := 7 + io.messageLength.resized
        frameBytes(0) := (frameLength - 2).asBits.resized // Frame length LSB
        frameBytes(1) := 0x00 // Frame length MSB
        frameBytes(2) := IServerProtocol.REQ_PUTS // Request type
        frameBytes(3) := 0x01 // Stream ID LSB
        frameBytes(4) := 0x00 // Stream ID
        frameBytes(5) := 0x00 // Stream ID
        frameBytes(6) := 0x00 // Stream ID MSB
        frameBytes(7) := io.messageLength.asBits.resized // Data length LSB
        frameBytes(8) := 0x00 // Data length MSB
        frameBytes(9) := io.message // Message byte
        byteIndex := 0
        goto(SEND_FRAME)
      }
    }

    SEND_FRAME.whenIsActive {
      iserver.io.linkIn.valid := True
      iserver.io.linkIn.payload := frameBytes(byteIndex)
      when(iserver.io.linkIn.ready) {
        byteIndex := byteIndex + 1
        when(byteIndex >= frameLength) {
          goto(DONE)
        }
      }
    }

    DONE.whenIsActive {
      io.sent := True
      when(!io.start) {
        goto(IDLE)
      }
    }
  }

  // Connect console output (for now just loop back)
  iserver.io.consoleOut.ready := True
  iserver.io.consoleIn.valid := False
  iserver.io.consoleIn.payload := 0
  iserver.io.fileOp.ready := True
}
