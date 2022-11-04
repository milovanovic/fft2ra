package rspChain

import chisel3._
import chisel3.util._

import chisel3.experimental._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import dsptools.numbers._
import freechips.rocketchip.interrupts._

import fft._
import dsputils._
import xWRDataPreProc._
import zeropadder._


case class FFT2DAngleParameters (
  angleFFTParams    : AngleFFTParameters,
  rangeFFTParams    : FFTParams[FixedPoint],
  preProcParams     : AXI4XwrDataPreProcParams,
  //dspQueueParams : SimpleDspQueueCustomParams,
  rangeLastGenParams: LastSignalGeneratorParameters,
  txSplitterParams  : AXI4StreamTxSplitterParameters,
  rangeFFTAddress   : AddressSet,
  preProcAddress    : AddressSet,
  txSplitterAddress : AddressSet,
  totalData         : Int,
  rxNum        : Int,
  txNum        : Int
)

class FFT2DAngleBlock (val params: FFT2DAngleParameters, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  
  val rangeFFT    = LazyModule(new AXI4MultipleFFTsBlock(params.rangeFFTParams, params.rangeFFTAddress, _beatBytes = beatBytes, configInterface = false))
  val angleFFTBlock = LazyModule(new AngleFFT(params.angleFFTParams, beatBytes = beatBytes))
  
  val xAWRpreProc = LazyModule(new AXI4xWRdataPreProcMultipleStreams(params.preProcAddress, params.preProcParams, beatBytes))
  
  //val dspQueues   = Seq.fill(1)(LazyModule(new SimpleDspQueue(params.dspQueueParams)))
  
  //val lastGenRange = Seq.fill(params.rxNum)(LazyModule(new LastSignalGenerator(params.rangeLastGenParams, beatBytes)))
  //val txSplitter = LazyModule(new AXI4StreamTxSplitter(params.txSplitterParams, beatBytes))
  val txSplitter = LazyModule(new AXI4TxSplitterBlock(params.txSplitterParams, params.txSplitterAddress, beatBytes))
  val rxJoiner = LazyModule(new AXI4StreamRxJoiner(params.rxNum, beatBytes))
  
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)

  angleFFTBlock.mem.get := AXI4Buffer() := bus.node
  rangeFFT.mem.get := AXI4Buffer() := bus.node
  xAWRpreProc.mem.get := AXI4Buffer() := bus.node
  txSplitter.mem.get := AXI4Buffer() := bus.node
  
  for (i <- 0 until params.rxNum) {
    //rangeFFT.streamNode := lastGenRange(i).streamNode
    rangeFFT.streamNode := xAWRpreProc.streamNode
    rxJoiner.inNode(i) := rangeFFT.streamNode
  }
  
  val streamBuffer = AXI4StreamBuffer()
  
  txSplitter.slaveNode := streamBuffer := rxJoiner.outNode

  for (i <- 0 until params.txNum) {
    angleFFTBlock.dspQueues(i).streamNode := txSplitter.masterNodes(i)
  }
  //dspQueues(0).streamNode := angleFFTBlock.angleFFT.streamNode
  
  lazy val module = new LazyModuleImp(this) {
      
  }
  
}


trait FFT2DAngleBlockPins extends FFT2DAngleBlock {
  
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until params.rxNum) yield {
    implicit val valName = ValName(s"in_$i")
    //val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    //lastGenRange(i).streamNode :=
    xAWRpreProc.streamNode :=
      //BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = 2))) :=
      in
    InModuleBody { in.makeIO() }
  }
  
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := angleFFTBlock.angleFFT.streamNode//dspQueues(0).streamNode
  val out = InModuleBody { ioOutNode.makeIO() }
  
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
}


object FFT2DAngleBlockApp extends App {
  
  val beatBytes = 4
  val rxNum = 4
  val txNum = 2
  val angleFFT = rxNum * txNum // 8 // 64
  val rangeFFT = 256
  val bpmMode = false
  val totalData = rangeFFT * angleFFT
  val params = FFT2DAngleParameters (
    angleFFTParams = AngleFFTParameters (
      FFTParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = angleFFT,
        useBitReverse  = true,
        runTime = true,
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
        packetSizeStart = angleFFT,
        packetSizeEnd  = angleFFT,
        queueDepth = 64,
        numberOfPackets = rangeFFT,
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
  val lazyModule = LazyModule(new FFT2DAngleBlock(params, beatBytes) with FFT2DAngleBlockPins)
  chisel3.Driver.execute(Array("--target-dir", "./verilog/FFT2DAngleBlock"), ()=> lazyModule.module)
}

