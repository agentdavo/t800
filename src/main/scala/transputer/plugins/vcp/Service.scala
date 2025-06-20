package transputer.plugins.vcp

import spinal.core._
import transputer.plugins.ChannelService

trait VcpService extends ChannelService {
  def scheduleInput(channel: Int): Unit
  def scheduleOutput(channel: Int): Unit
  def getChannelState(channel: Int): Bits
  def setVcpCommand(cmd: Bits): Unit
  def getVcpStatus(): Bits
  def sendPacket(channel: Int, data: Bits, isEnd: Bool): Unit
  def receiveAck(channel: Int): Bool
  def enqueueMessage(channel: Int, data: Bits): Unit
}
