package rspChain

import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import scala.io.Source
import java.io._
import scala.math.{pow, sqrt}
import breeze.plot._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}

import fft._
import dsputils._
import zeropadder._
import xWRDataPreProc._


class FFT2DAngleWithInputMemoryTester(   
  dut: FFT2DAngleWithInputMemory  with FFT2DAngleWithInputMemoryPins,
  beatBytes: Int
)  extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel {
  
  def memAXI: AXI4Bundle = dut.ioMem.get
  
  //poke(dut.reset, 1)
  poke(dut.out.ready, 0)
  
  step(5)
  
  //poke(dut.reset, 0)
  poke(dut.out.ready, 1)
  
  step(15000)

}


class FFT2DAngleWithInputMemorySpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 4
  val rxNum = 4
  val txNum = 2
  val angleFFT = rxNum * txNum // 8 // 64
  val rangeFFT = 1024
  val bpmMode = false
  val totalData = rangeFFT * angleFFT
  val params = FFT2DAngleParameters (
    angleFFTParams = AngleFFTParameters (
      FFTParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = angleFFT,
        useBitReverse  = true,
        runTime = false,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        sdfRadix = "2",
        trimType = Convergent,
        expandLogic = Array.fill(log2Up(angleFFT))(0),
        keepMSBorLSB = Array.fill(log2Up(angleFFT))(true),
        minSRAMdepth = 128, // memories larger than 64 should be mapped on block ram
        binPoint = 14
      ),
      dspQueueParams = SimpleDspQueueCustomParams(queueDepth = 2*rangeFFT,
                        useSyncReadMem = false,
                        useBlockRam = false),
      PreAngleFFTParams  = PreAngleFFTBlockParameters(
        maxFFTSize = rangeFFT, 
        rxNum = rxNum,
        txNum = txNum,
        bpmMode = bpmMode
      ),
      /*angleLastGenParams = LastSignalGeneratorParameters(
        maxTotalNumSize = 16384,
        totalDataNum = rangeFFT * rxNum * txNum
      ),*/
      zeroPadderParams = ZeroPadderParams(
        proto = FixedPoint(16.W, 14.BP),
        packetSizeStart = 8,
        packetSizeEnd  = 8,
        queueDepth = 64,
        numberOfPackets = 1024,
        useQueue = false,
        isDataComplex = true,
        useBlockRam = false
      ),
      bpmParams = BPMParams(
        maxFFTSize = rangeFFT, 
        queueSize = rangeFFT, 
        rxNum = rxNum, 
        bpmMode = bpmMode
      ),
      FFTAddress    = AddressSet(0x50000000 , 0xFF),
      bpmAddress    = AddressSet(0x50000100 , 0xFF),
      zeroPadderAddress = AddressSet(0x50000200 , 0xFF),
      rxNum = rxNum,
      txNum = txNum,
      FFTSize = angleFFT,
      bpmMode = bpmMode
    ),
    rangeFFTParams = FFTParams.fixed(
      dataWidth = 16,
      twiddleWidth = 16,
      numPoints = rangeFFT,
      useBitReverse  = true,
      runTime = true,
      numAddPipes = 1,
      numMulPipes = 1,
      use4Muls = true,
      expandLogic = Array.fill(log2Up(rangeFFT))(0),
      sdfRadix = "2",
      trimType = Convergent,
      keepMSBorLSB = Array.fill(log2Up(rangeFFT))(true),
      minSRAMdepth = 128, // memories larger than 64 should be mapped on block ram
      binPoint = 14
    ),
    preProcParams  = AXI4XwrDataPreProcParams(maxFFTSize = rangeFFT, useBlockRam = true),
    dspQueueParams = SimpleDspQueueCustomParams(queueDepth = 256,
                        useSyncReadMem = false,
                        useBlockRam = false),
    rangeLastGenParams = LastSignalGeneratorParameters(
      maxTotalNumSize = 4096,
      totalDataNum = rangeFFT * txNum
    ),
    txSplitterParams = AXI4StreamTxSplitterParameters(
      maxFFTSize = 8192,
      FFTSize = rangeFFT,
      txNum = txNum,
      rxNum = rxNum
    ),
    rangeFFTAddress    = AddressSet(0x50000300 , 0xFF),
    preProcAddress     = AddressSet(0x50000400, 0xFF),
    txSplitterAddress = AddressSet(0x50000500, 0xFF),
    totalData          = totalData,
    rxNum = rxNum,
    txNum = txNum
  )
  
  it should "Test " in {
    val lazyDut = LazyModule(new FFT2DAngleWithInputMemory(params, beatBytes) with FFT2DAngleWithInputMemoryPins {
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new FFT2DAngleWithInputMemoryTester(lazyDut, beatBytes)
    } should be (true)
  }
}
