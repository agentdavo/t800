package t800

object TConsts {
  val WordBits = 32
  val AddrBits = 32
  val RomWords = 16
  val RamWords = 4096
  val MicroWords = 1024
  val LinkCount = 4
  val ResetPC = 0x0000_0000L

  // Memory layout
  val InternalMemStart = 0x80000000L
  val InternalMemEnd = 0x80000fffL
  val ExternalMemStart = 0x80001000L
  val MemStart = 0x80000070L

  val MaxINT = 0x7fffffffL
  val ResetCode = 0x7ffffffeL
  val EregIntSaveLoc = 0x80000044L
  val StatusIntSaveLoc = 0x80000040L
  val CregIntSaveLoc = 0x8000003cL
  val BregIntSaveLoc = 0x80000038L
  val AregIntSaveLoc = 0x80000034L
  val IptrIntSaveLoc = 0x80000030L
  val WdescIntSaveLoc = 0x8000002cL
  val TPtrLoc1 = 0x80000028L
  val TPtrLoc0 = 0x80000024L
  val EventLoc = 0x80000020L
  val Link3Input = 0x8000001cL
  val Link2Input = 0x80000018L
  val Link1Input = 0x80000014L
  val Link0Input = 0x80000010L
  val Link3Output = 0x8000000cL
  val Link2Output = 0x80000008L
  val Link1Output = 0x80000004L
  val Link0Output = 0x80000000L
}
