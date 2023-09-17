/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import difftest._
import top.Settings
import chisel3.util.experimental.loadMemoryFromFile
import firrtl.annotations.MemoryLoadFileType

object ALUOpType {
  def add  = "b1000000".U
  def sll  = "b0000001".U
  def slt  = "b0000010".U
  def sltu = "b0000011".U
  def xor  = "b0000100".U
  def srl  = "b0000101".U
  def or   = "b0000110".U
  def and  = "b0000111".U
  def sub  = "b0001000".U
  def sra  = "b0001101".U

  def addw = "b1100000".U
  def subw = "b0101000".U
  def sllw = "b0100001".U
  def srlw = "b0100101".U
  def sraw = "b0101101".U

  def isWordOp(func: UInt) = func(5)

  def jal  = "b1011000".U
  def jalr = "b1011010".U
  def beq  = "b0010000".U
  def bne  = "b0010001".U
  def blt  = "b0010100".U
  def bge  = "b0010101".U
  def bltu = "b0010110".U
  def bgeu = "b0010111".U

  // for RAS
  def call = "b1011100".U
  def ret  = "b1011110".U

  // crc
  def crc32w= "b1001111".U
  def crc32 = "b0001111".U

  def isAdd(func: UInt) = func(6)
  def isWordCrc(func: UInt) = func(6)
  def pcPlus2(func: UInt) = func(5)
  def isBru(func: UInt) = func(4)
  def isBranch(func: UInt) = !func(3)
  def isJump(func: UInt) = isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 1)
  def isBranchInvert(func: UInt) = func(0)
}

class ALUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
  val offset = Input(UInt(XLEN.W))
}

class ALU(hasBru: Boolean = false) extends NutCoreModule {
  val io = IO(new ALUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  // crc32 lookup table, rom
  val crcTab = Array(
    0x00000000L.U(32.W), 0x77073096L.U(32.W), 0xee0e612cL.U(32.W), 0x990951baL.U(32.W), 0x076dc419L.U(32.W), 0x706af48fL.U(32.W),
    0xe963a535L.U(32.W), 0x9e6495a3L.U(32.W), 0x0edb8832L.U(32.W), 0x79dcb8a4L.U(32.W), 0xe0d5e91eL.U(32.W), 0x97d2d988L.U(32.W),
    0x09b64c2bL.U(32.W), 0x7eb17cbdL.U(32.W), 0xe7b82d07L.U(32.W), 0x90bf1d91L.U(32.W), 0x1db71064L.U(32.W), 0x6ab020f2L.U(32.W),
    0xf3b97148L.U(32.W), 0x84be41deL.U(32.W), 0x1adad47dL.U(32.W), 0x6ddde4ebL.U(32.W), 0xf4d4b551L.U(32.W), 0x83d385c7L.U(32.W),
    0x136c9856L.U(32.W), 0x646ba8c0L.U(32.W), 0xfd62f97aL.U(32.W), 0x8a65c9ecL.U(32.W), 0x14015c4fL.U(32.W), 0x63066cd9L.U(32.W),
    0xfa0f3d63L.U(32.W), 0x8d080df5L.U(32.W), 0x3b6e20c8L.U(32.W), 0x4c69105eL.U(32.W), 0xd56041e4L.U(32.W), 0xa2677172L.U(32.W),
    0x3c03e4d1L.U(32.W), 0x4b04d447L.U(32.W), 0xd20d85fdL.U(32.W), 0xa50ab56bL.U(32.W), 0x35b5a8faL.U(32.W), 0x42b2986cL.U(32.W),
    0xdbbbc9d6L.U(32.W), 0xacbcf940L.U(32.W), 0x32d86ce3L.U(32.W), 0x45df5c75L.U(32.W), 0xdcd60dcfL.U(32.W), 0xabd13d59L.U(32.W),
    0x26d930acL.U(32.W), 0x51de003aL.U(32.W), 0xc8d75180L.U(32.W), 0xbfd06116L.U(32.W), 0x21b4f4b5L.U(32.W), 0x56b3c423L.U(32.W),
    0xcfba9599L.U(32.W), 0xb8bda50fL.U(32.W), 0x2802b89eL.U(32.W), 0x5f058808L.U(32.W), 0xc60cd9b2L.U(32.W), 0xb10be924L.U(32.W),
    0x2f6f7c87L.U(32.W), 0x58684c11L.U(32.W), 0xc1611dabL.U(32.W), 0xb6662d3dL.U(32.W), 0x76dc4190L.U(32.W), 0x01db7106L.U(32.W),
    0x98d220bcL.U(32.W), 0xefd5102aL.U(32.W), 0x71b18589L.U(32.W), 0x06b6b51fL.U(32.W), 0x9fbfe4a5L.U(32.W), 0xe8b8d433L.U(32.W),
    0x7807c9a2L.U(32.W), 0x0f00f934L.U(32.W), 0x9609a88eL.U(32.W), 0xe10e9818L.U(32.W), 0x7f6a0dbbL.U(32.W), 0x086d3d2dL.U(32.W),
    0x91646c97L.U(32.W), 0xe6635c01L.U(32.W), 0x6b6b51f4L.U(32.W), 0x1c6c6162L.U(32.W), 0x856530d8L.U(32.W), 0xf262004eL.U(32.W),
    0x6c0695edL.U(32.W), 0x1b01a57bL.U(32.W), 0x8208f4c1L.U(32.W), 0xf50fc457L.U(32.W), 0x65b0d9c6L.U(32.W), 0x12b7e950L.U(32.W),
    0x8bbeb8eaL.U(32.W), 0xfcb9887cL.U(32.W), 0x62dd1ddfL.U(32.W), 0x15da2d49L.U(32.W), 0x8cd37cf3L.U(32.W), 0xfbd44c65L.U(32.W),
    0x4db26158L.U(32.W), 0x3ab551ceL.U(32.W), 0xa3bc0074L.U(32.W), 0xd4bb30e2L.U(32.W), 0x4adfa541L.U(32.W), 0x3dd895d7L.U(32.W),
    0xa4d1c46dL.U(32.W), 0xd3d6f4fbL.U(32.W), 0x4369e96aL.U(32.W), 0x346ed9fcL.U(32.W), 0xad678846L.U(32.W), 0xda60b8d0L.U(32.W),
    0x44042d73L.U(32.W), 0x33031de5L.U(32.W), 0xaa0a4c5fL.U(32.W), 0xdd0d7cc9L.U(32.W), 0x5005713cL.U(32.W), 0x270241aaL.U(32.W),
    0xbe0b1010L.U(32.W), 0xc90c2086L.U(32.W), 0x5768b525L.U(32.W), 0x206f85b3L.U(32.W), 0xb966d409L.U(32.W), 0xce61e49fL.U(32.W),
    0x5edef90eL.U(32.W), 0x29d9c998L.U(32.W), 0xb0d09822L.U(32.W), 0xc7d7a8b4L.U(32.W), 0x59b33d17L.U(32.W), 0x2eb40d81L.U(32.W),
    0xb7bd5c3bL.U(32.W), 0xc0ba6cadL.U(32.W), 0xedb88320L.U(32.W), 0x9abfb3b6L.U(32.W), 0x03b6e20cL.U(32.W), 0x74b1d29aL.U(32.W),
    0xead54739L.U(32.W), 0x9dd277afL.U(32.W), 0x04db2615L.U(32.W), 0x73dc1683L.U(32.W), 0xe3630b12L.U(32.W), 0x94643b84L.U(32.W),
    0x0d6d6a3eL.U(32.W), 0x7a6a5aa8L.U(32.W), 0xe40ecf0bL.U(32.W), 0x9309ff9dL.U(32.W), 0x0a00ae27L.U(32.W), 0x7d079eb1L.U(32.W),
    0xf00f9344L.U(32.W), 0x8708a3d2L.U(32.W), 0x1e01f268L.U(32.W), 0x6906c2feL.U(32.W), 0xf762575dL.U(32.W), 0x806567cbL.U(32.W),
    0x196c3671L.U(32.W), 0x6e6b06e7L.U(32.W), 0xfed41b76L.U(32.W), 0x89d32be0L.U(32.W), 0x10da7a5aL.U(32.W), 0x67dd4accL.U(32.W),
    0xf9b9df6fL.U(32.W), 0x8ebeeff9L.U(32.W), 0x17b7be43L.U(32.W), 0x60b08ed5L.U(32.W), 0xd6d6a3e8L.U(32.W), 0xa1d1937eL.U(32.W),
    0x38d8c2c4L.U(32.W), 0x4fdff252L.U(32.W), 0xd1bb67f1L.U(32.W), 0xa6bc5767L.U(32.W), 0x3fb506ddL.U(32.W), 0x48b2364bL.U(32.W),
    0xd80d2bdaL.U(32.W), 0xaf0a1b4cL.U(32.W), 0x36034af6L.U(32.W), 0x41047a60L.U(32.W), 0xdf60efc3L.U(32.W), 0xa867df55L.U(32.W),
    0x316e8eefL.U(32.W), 0x4669be79L.U(32.W), 0xcb61b38cL.U(32.W), 0xbc66831aL.U(32.W), 0x256fd2a0L.U(32.W), 0x5268e236L.U(32.W),
    0xcc0c7795L.U(32.W), 0xbb0b4703L.U(32.W), 0x220216b9L.U(32.W), 0x5505262fL.U(32.W), 0xc5ba3bbeL.U(32.W), 0xb2bd0b28L.U(32.W),
    0x2bb45a92L.U(32.W), 0x5cb36a04L.U(32.W), 0xc2d7ffa7L.U(32.W), 0xb5d0cf31L.U(32.W), 0x2cd99e8bL.U(32.W), 0x5bdeae1dL.U(32.W),
    0x9b64c2b0L.U(32.W), 0xec63f226L.U(32.W), 0x756aa39cL.U(32.W), 0x026d930aL.U(32.W), 0x9c0906a9L.U(32.W), 0xeb0e363fL.U(32.W),
    0x72076785L.U(32.W), 0x05005713L.U(32.W), 0x95bf4a82L.U(32.W), 0xe2b87a14L.U(32.W), 0x7bb12baeL.U(32.W), 0x0cb61b38L.U(32.W),
    0x92d28e9bL.U(32.W), 0xe5d5be0dL.U(32.W), 0x7cdcefb7L.U(32.W), 0x0bdbdf21L.U(32.W), 0x86d3d2d4L.U(32.W), 0xf1d4e242L.U(32.W),
    0x68ddb3f8L.U(32.W), 0x1fda836eL.U(32.W), 0x81be16cdL.U(32.W), 0xf6b9265bL.U(32.W), 0x6fb077e1L.U(32.W), 0x18b74777L.U(32.W),
    0x88085ae6L.U(32.W), 0xff0f6a70L.U(32.W), 0x66063bcaL.U(32.W), 0x11010b5cL.U(32.W), 0x8f659effL.U(32.W), 0xf862ae69L.U(32.W),
    0x616bffd3L.U(32.W), 0x166ccf45L.U(32.W), 0xa00ae278L.U(32.W), 0xd70dd2eeL.U(32.W), 0x4e048354L.U(32.W), 0x3903b3c2L.U(32.W),
    0xa7672661L.U(32.W), 0xd06016f7L.U(32.W), 0x4969474dL.U(32.W), 0x3e6e77dbL.U(32.W), 0xaed16a4aL.U(32.W), 0xd9d65adcL.U(32.W),
    0x40df0b66L.U(32.W), 0x37d83bf0L.U(32.W), 0xa9bcae53L.U(32.W), 0xdebb9ec5L.U(32.W), 0x47b2cf7fL.U(32.W), 0x30b5ffe9L.U(32.W),
    0xbdbdf21cL.U(32.W), 0xcabac28aL.U(32.W), 0x53b39330L.U(32.W), 0x24b4a3a6L.U(32.W), 0xbad03605L.U(32.W), 0xcdd70693L.U(32.W),
    0x54de5729L.U(32.W), 0x23d967bfL.U(32.W), 0xb3667a2eL.U(32.W), 0xc4614ab8L.U(32.W), 0x5d681b02L.U(32.W), 0x2a6f2b94L.U(32.W),
    0xb40bbe37L.U(32.W), 0xc30c8ea1L.U(32.W), 0x5a05df1bL.U(32.W), 0x2d02ef8dL.U(32.W)
  )
  val crcTable = RegInit(VecInit(List.tabulate(256)(n => crcTab(n))))

  val tabAcc = Wire(UInt(64.W))
  val crcSrl = Wire(UInt(64.W))
  tabAcc    := ZeroExt(crcTable((src1 ^ src2) & 0xff.U), 64)
  crcSrl    := Mux(ALUOpType.isWordCrc(func), ZeroExt(src2(31, 0), 64), src2) >> 8.U
  val crcRes = tabAcc ^ crcSrl
  val isAdderSub = !ALUOpType.isAdd(func)
  val adderRes = (src1 +& (src2 ^ Fill(XLEN, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val shsrc1 = LookupTreeDefault(func, src1(XLEN-1,0), List(
    ALUOpType.srlw -> ZeroExt(src1(31,0), XLEN),
    ALUOpType.sraw -> SignExt(src1(31,0), XLEN)
  ))
  val shamt = Mux(ALUOpType.isWordOp(func), src2(4, 0), if (XLEN == 64) src2(5, 0) else src2(4, 0))
  val res = LookupTreeDefault(func(3, 0), adderRes, List(
    ALUOpType.sll  -> ((shsrc1  << shamt)(XLEN-1, 0)),
    ALUOpType.slt  -> ZeroExt(slt, XLEN),
    ALUOpType.sltu -> ZeroExt(sltu, XLEN),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (shsrc1  >> shamt),
    ALUOpType.or   -> (src1  |  src2),
    ALUOpType.and  -> (src1  &  src2),
    ALUOpType.sra  -> ((shsrc1.asSInt >> shamt).asUInt),
    ALUOpType.crc32-> crcRes
  ))
  val aluRes = Mux(ALUOpType.isWordOp(func), SignExt(res(31,0), 64), res)

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )

  val isBranch = ALUOpType.isBranch(func)
  val isBru = ALUOpType.isBru(func)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, io.cfIn.pc + io.offset, adderRes)(VAddrBits-1,0)
  val predictWrong = Mux(!taken && isBranch, io.cfIn.brIdx(0), !io.cfIn.brIdx(0) || (io.redirect.target =/= io.cfIn.pnpc))
  val isRVC = (io.cfIn.instr(1,0) =/= "b11".U)
  assert(io.cfIn.instr(1,0) === "b11".U || isRVC || !valid)
  Debug(valid && (io.cfIn.instr(1,0) === "b11".U) =/= !isRVC, "[ERROR] pc %x inst %x rvc %x\n",io.cfIn.pc, io.cfIn.instr, isRVC)
  io.redirect.target := Mux(!taken && isBranch, Mux(isRVC, io.cfIn.pc + 2.U, io.cfIn.pc + 4.U), target)
  // with branch predictor, this is actually to fix the wrong prediction
  io.redirect.valid := valid && isBru && predictWrong
  val redirectRtype = if (EnableOutOfOrderExec) 1.U else 0.U
  io.redirect.rtype := redirectRtype
  // mark redirect type as speculative exec fix
  // may be can be moved to ISU to calculate pc + 4
  // this is actually for jal and jalr to write pc + 4/2 to rd
  io.out.bits := Mux(isBru, Mux(!isRVC, SignExt(io.cfIn.pc, AddrBits) + 4.U, SignExt(io.cfIn.pc, AddrBits) + 2.U), aluRes)
  //Debug(func === ALUOpType.crc32, "crc32 detected: src1=%x, src2=%x, tabAcc=%x, crcSrl=%x, crcRes=%x, aluRes=%x, io.out.bits=%x\n", src1, src2, tabAcc, crcSrl, crcRes, aluRes, io.out.bits)
  Debug(valid && isBru, "tgt %x, valid:%d, npc: %x, pdwrong: %x\n", io.redirect.target, io.redirect.valid, io.cfIn.pnpc, predictWrong)
  Debug(valid && isBru, "taken:%d addrRes:%x src1:%x src2:%x func:%x\n", taken, adderRes, src1, src2, func)
  Debug(valid && isBru, "[BPW] pc %x tgt %x, npc: %x, pdwrong: %x type: %x%x%x%x\n", io.cfIn.pc, io.redirect.target, io.cfIn.pnpc, predictWrong, isBranch, (func === ALUOpType.jal || func === ALUOpType.call), func === ALUOpType.jalr, func === ALUOpType.ret)
  Debug("valid:%d isBru:%d isBranch:%d \n", valid, isBru, isBranch)
  // Debug("pc %x instr %x tgt %x, npc: %x, pdwrong: %x type: %x%x%x%x\n", io.cfIn.pc, io.cfIn.instr, io.redirect.target, io.cfIn.pnpc, predictWrong, isBranch, (func === ALUOpType.jal || func === ALUOpType.call), func === ALUOpType.jalr, func === ALUOpType.ret)
  // Debug("func:%b ", func)
  // Debug("tgt %x, npc: %x, pdwrong: %x\n", io.redirect.target, io.cfIn.pnpc, predictWrong)
  // Debug("taken:%d addrRes:%x src1:%x src2:%x func:%x\n", taken, adderRes, src1, src2, func)

  Debug(valid && isBru, " bpuUpdateReq: valid:%d pc:%x isMissPredict:%d actualTarget:%x actualTaken:%x fuOpType:%x btbType:%x isRVC:%d \n", valid && isBru, io.cfIn.pc, predictWrong, target, taken, func, LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable), isRVC)

  io.in.ready := io.out.ready
  io.out.valid := valid

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid && isBru
  bpuUpdateReq.pc := io.cfIn.pc
  bpuUpdateReq.isMissPredict := predictWrong
  bpuUpdateReq.actualTarget := target
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable)
  bpuUpdateReq.isRVC := isRVC

  if(hasBru){
    BoringUtils.addSource(RegNext(bpuUpdateReq), "bpuUpdateReq")
  
    val right = valid && isBru && !predictWrong
    val wrong = valid && isBru && predictWrong
    BoringUtils.addSource(right && isBranch, "MbpBRight")
    BoringUtils.addSource(wrong && isBranch, "MbpBWrong")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h0".U && isRVC, "Custom1")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h0".U && !isRVC, "Custom2")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h2".U && isRVC, "Custom3")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h2".U && !isRVC, "Custom4")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h4".U && isRVC, "Custom5")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h4".U && !isRVC, "Custom6")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h6".U && isRVC, "Custom7")
    BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h6".U && !isRVC, "Custom8")
    BoringUtils.addSource(right && (func === ALUOpType.jal || func === ALUOpType.call), "MbpJRight")
    BoringUtils.addSource(wrong && (func === ALUOpType.jal || func === ALUOpType.call), "MbpJWrong")
    BoringUtils.addSource(right && func === ALUOpType.jalr, "MbpIRight")
    BoringUtils.addSource(wrong && func === ALUOpType.jalr, "MbpIWrong")
    BoringUtils.addSource(right && func === ALUOpType.ret, "MbpRRight")
    BoringUtils.addSource(wrong && func === ALUOpType.ret, "MbpRWrong")
  }
}
