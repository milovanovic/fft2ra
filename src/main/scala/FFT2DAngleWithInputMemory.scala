package rspChain

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import chisel3.experimental._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import dsptools.numbers._
import freechips.rocketchip.interrupts._

import fft._
import dsputils._
import zeropadder._
import xWRDataPreProc._


class FFT2DAngleWithInputMemory (val params: FFT2DAngleParameters, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  
  val fft2d = LazyModule(new FFT2DAngleBlock(params, beatBytes))
  //val gbemac = LazyModule(new GbemacWrapper(gbemacAddressSet, beatBytes))
  val memoryModule = LazyModule(new FileMemoryModule(params.angleFFTParams.bpmMode, beatBytes))
  //val memoryModule = LazyModule(new FileMemoryModuleJoint(params.angleFFTParams.bpmMode, beatBytes))
  
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  
  fft2d.mem.get := AXI4Buffer() := bus.node
  //gbemac.mem.get := AXI4Buffer() := bus.node
  
  //gbemac.streamNode.get := fft2d.dspQueues(0).streamNode
  
  /*fft2d.lastGenRange(0).streamNode := memoryModule.outNode0
  fft2d.lastGenRange(1).streamNode := memoryModule.outNode1
  fft2d.lastGenRange(2).streamNode := memoryModule.outNode2
  fft2d.lastGenRange(3).streamNode := memoryModule.outNode3*/
  
  //fft2d.xAWRpreProc(0).streamNode := memoryModule.outNode0
  //fft2d.xAWRpreProc(1).streamNode := memoryModule.outNode1
  //fft2d.xAWRpreProc(2).streamNode := memoryModule.outNode2
  
  //fft2d.xAWRpreProc.streamNode := memoryModule.outNode
  
  //for (i <- 0 until params.rxNum) yield {
  //  fft2d.xAWRpreProc.streamNode := memoryModule.outNode
  //}
  
  for (i <- 0 until params.rxNum) yield {
    fft2d.xAWRpreProc.streamNode := memoryModule.streamNodes(i)
  }
  
  lazy val module = new LazyModuleImp(this) {
      //val io = IO(new GbemacWrapperIO)
      //io <> gbemac.module.io
  }

}


trait FFT2DAngleWithInputMemoryPins extends FFT2DAngleWithInputMemory {
  
  /*val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until params.rxNum) yield {
    implicit val valName = ValName(s"in_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    fft2d.lastGenRange(i).streamNode :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) :=
      in
    InModuleBody { in.makeIO() }
  }*/
  
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := fft2d.angleFFTBlock.angleFFT.streamNode // fft2d.dspQueues(0).streamNode
  val out = InModuleBody { ioOutNode.makeIO() }
  
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

}


object FFT2DAngleWithInputMemoryApp extends App {
  
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
    /*dspQueueParams = SimpleDspQueueCustomParams(queueDepth = 256,
                        useSyncReadMem = false,
                        useBlockRam = false),*/
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
  implicit val p: Parameters = Parameters.empty
  val lazyModule = LazyModule(new FFT2DAngleWithInputMemory(params, beatBytes) with FFT2DAngleWithInputMemoryPins)
  chisel3.Driver.execute(Array("--target-dir", "./verilog/FFT2DAngleWithInputMemory"), ()=> lazyModule.module)
}
