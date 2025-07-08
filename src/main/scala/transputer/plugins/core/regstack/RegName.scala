package transputer.plugins.core.regstack

import spinal.core._

object RegName extends SpinalEnum {
  val Areg, Breg, Creg, WdescReg, IptrReg, StatusReg, ThReg, FPstatusReg, FPAreg, FPBreg, FPCreg,
    BMreg0, BMreg1, BMreg2, WIReg, WuReg, Ereg, Xreg, EptrReg, RegionReg0, RegionReg1, RegionReg2,
    RegionReg3, PstateReg, WdescStubReg, FptrReg0, FptrReg1, BptrReg0, BptrReg1, ClockReg0,
    ClockReg1, TptrReg0, TptrReg1, TnextReg0, TnextReg1 = newElement()
}
