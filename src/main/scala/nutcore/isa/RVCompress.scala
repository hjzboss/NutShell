package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

/**
  * Custom compression acceleration extension instructions, 
  * including CRC32 instructions, multiply hash instructions,
  * and vector adjacent instructions.
  */
object RVCompressInstr extends HasInstrType {
  def CRC32         = BitPat("b0100000_?????_?????_111_?????_1111011")

  def MULHASH32     = BitPat("b?????00_?????_?????_010_?????_0001011")
  def MULHASH64     = BitPat("b?????01_?????_?????_010_?????_0001011")

  def VFILLR_VX     = BitPat("b000000?_?????_?????_100_?????_0001011")

  def VAND_ADJ_VX   = BitPat("b110000?_?????_?????_111_?????_1111011")
  def VOR_ADJ_VX    = BitPat("b110001?_?????_?????_111_?????_1111011")
  def VXOR_ADJ_VX   = BitPat("b110010?_?????_?????_111_?????_1111011")
  def VADD_ADJ_VX   = BitPat("b110011?_?????_?????_111_?????_1111011")
  def VSUB_ADJ_VX   = BitPat("b110100?_?????_?????_111_?????_1111011")

  val table = Array(
    // TODO: COMPRESS
  )
}
