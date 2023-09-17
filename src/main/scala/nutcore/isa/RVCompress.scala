package nutcore

import chisel3._
import chisel3.util._

import utils._

/**
  * Custom compression acceleration extension instructions, 
  * including CRC32 instructions, multiply hash instructions.
  */
object RVCompressInstr extends HasInstrType {
  // TODO: crc32 unsigned, crc32 unsigned long, crc32 int, crc32 long
  def CRC32W        = BitPat("b0100000??????????111?????1111011") // rs2 is unsigned
  def CRC32         = BitPat("b0000000??????????111?????1111011")
  def MULHASH32     = BitPat("b?????00??????????010?????0001011")
  def MULHASH64     = BitPat("b?????01??????????010?????0001011")

  val table = Array(
    CRC32W         -> List(InstrR, FuType.alu, ALUOpType.crc32w),
    CRC32          -> List(InstrR, FuType.alu, ALUOpType.crc32),
    MULHASH32      -> List(InstrR4, FuType.mdu, MDUOpType.mulhash32),
    MULHASH64      -> List(InstrR4, FuType.mdu, MDUOpType.mulhash64),
  )
}
