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
import zeropadder._


case class AngleFFTParameters (
  dspQueueParams : SimpleDspQueueCustomParams,
  FFTParams : FFTParams[FixedPoint],
  PreAngleFFTParams : PreAngleFFTBlockParameters,
  //angleLastGenParams: LastSignalGeneratorParameters,
  zeroPadderParams: ZeroPadderParams[FixedPoint],
  bpmParams : BPMParams,
  FFTAddress : AddressSet,
  bpmAddress : AddressSet,
  zeroPadderAddress : AddressSet,
  FFTSize : Int = 8,
  rxNum : Int = 4,
  txNum : Int = 4,
  bpmMode : Boolean = false
)

class AngleFFT (val params: AngleFFTParameters, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  
  val angleFFT = LazyModule(new AXI4FFTBlock(params.FFTParams, params.FFTAddress, _beatBytes = beatBytes, configInterface = false))
  
  val dspQueues   = Seq.fill(params.txNum)(LazyModule(new SimpleDspQueue(params.dspQueueParams)))
  
  val preAngleFFT = LazyModule(new PreAngleFFTBlock(params.PreAngleFFTParams, beatBytes))
  //val lastGenAngle = LazyModule(new LastSignalGenerator(params.angleLastGenParams, beatBytes))
  val zeroPadder = LazyModule(new AXI4ZeroPadderBlock(params.zeroPadderParams, params.zeroPadderAddress, beatBytes))
  val bpmDemodulation = LazyModule(new AXI4BPMDemodulationBlock(params.bpmParams, params.bpmAddress, beatBytes))
  
  val adapterForAngleFFT = AXI4StreamWidthAdapter.oneToN(params.rxNum)
  
  val streamBuffer = AXI4StreamBuffer()
  
  for (i <- 0 to (params.txNum - 1)) {
    preAngleFFT.slaveNodes(i) := dspQueues(i).streamNode
  }
  //angleFFT.streamNode := lastGenAngle.streamNode := adapterForAngleFFT := streamBuffer := bpmDemodulation.streamNode := preAngleFFT.masterNode
  angleFFT.streamNode := zeroPadder.streamNode := adapterForAngleFFT := streamBuffer := bpmDemodulation.streamNode := preAngleFFT.masterNode
  
  
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  angleFFT.mem.get := AXI4Buffer() := bus.node
  bpmDemodulation.mem.get := AXI4Buffer() := bus.node
  zeroPadder.mem.get := AXI4Buffer() := bus.node
  
  lazy val module = new LazyModuleImp(this) {
      
  }
  
}


trait AngleFFTPins extends AngleFFT {
  //def beatBytes: Int = 4
  
  
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until params.txNum) yield {
      implicit val valName = ValName(s"inIOs_$i")
      val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes * params.rxNum)))
      dspQueues(i).streamNode :=
        BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes * params.rxNum))) :=
        in
      InModuleBody { in.makeIO() }
    }
  
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := angleFFT.streamNode
  val out = InModuleBody { ioOutNode.makeIO() }
  
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
}


object AngleFFTApp extends App {
  
  val beatBytes = 4
  val angleFFT = 8 // 64
  val rangeFFT = 1024
  val bpmMode = false
  val rxNum = 4
  val txNum = 4
  val params = AngleFFTParameters (
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
    dspQueueParams = SimpleDspQueueCustomParams(queueDepth = 512,
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
  )

  implicit val p: Parameters = Parameters.empty
  val lazyModule = LazyModule(new AngleFFT(params, beatBytes) with AngleFFTPins)
  chisel3.Driver.execute(Array("--target-dir", "./verilog/AngleFFT"), ()=> lazyModule.module)
}

