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
import gbemac._
import zeropadder._
import xWRDataPreProc._
import magnitude._
import accumulator._


case class FFT2DAngleChainParameters (
  FFT2DAngleParams : FFT2DAngleParameters,
  accParams    : AccParams[FixedPoint],
  magParams    : MAGParams[FixedPoint],
  accAddress    : AddressSet,
  magAddress : AddressSet
)


class FFT2DAngleChainWithGbemac (val params: FFT2DAngleChainParameters, val gbemacAddressSet: AddressSet, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  
  val fft2d = LazyModule(new FFT2DAngleBlock(params.FFT2DAngleParams, beatBytes))
  val gbemac = LazyModule(new GbemacWrapper(gbemacAddressSet, beatBytes))
  val accumulator = LazyModule(new AXI4AccumulatorBlock(params.accParams, params.accAddress, beatBytes))
  val mag = LazyModule(new AXI4LogMagMuxBlock(params.magParams, params.magAddress, beatBytes))
  //val lastGen = LazyModule(new LastSignalGenerator(LastSignalGeneratorParameters(256*32*32, 256*32*32), 2))
  
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  val adapter16to32 = AXI4StreamWidthAdapter.nToOne(2)
  val streamBuffer = AXI4StreamBuffer()
  
  fft2d.mem.get := AXI4Buffer() := bus.node
  gbemac.mem.get := AXI4Buffer() := bus.node
  accumulator.mem.get := AXI4Buffer() := bus.node
  mag.mem.get := AXI4Buffer() := bus.node
  
  gbemac.streamNode.get := adapter16to32 := streamBuffer := accumulator.streamNode := mag.streamNode := fft2d.angleFFTBlock.angleFFT.streamNode // fft2d.dspQueues(0).streamNode
  //gbemac.streamNode.get := adapter16to32 := streamBuffer := mag.streamNode := fft2d.angleFFTBlock.angleFFT.streamNode // fft2d.dspQueues(0).streamNode
  //gbemac.streamNode.get := adapter16to32 := streamBuffer := accumulator.streamNode := lastGen.streamNode := mag.streamNode := fft2d.angleFFTBlock.angleFFT.streamNode // fft2d.dspQueues(0).streamNode
  
  lazy val module = new LazyModuleImp(this) {
      val io = IO(new GbemacWrapperIO)
      io <> gbemac.module.io
  }

}


trait FFT2DAngleChainWithGbemacPins extends FFT2DAngleChainWithGbemac {
  
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until params.FFT2DAngleParams.rxNum) yield {
    implicit val valName = ValName(s"in_$i")
    //val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    //fft2d.lastGenRange(i).streamNode :=
    fft2d.xAWRpreProc.streamNode :=
      //BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = 2))) :=
      in
    InModuleBody { in.makeIO() }
  }
  
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

}


object FFT2DAngleChainWithGbemacApp extends App {
  
  val beatBytes = 4
  val rxNum = 4
  val txNum = 2
  val angleFFT = rxNum * txNum // 8 // 64
  val rangeFFT = 256//1024
  val bpmMode = false
  val totalData = rangeFFT * 32 //angleFFT
  val params = FFT2DAngleChainParameters (
    FFT2DAngleParams = FFT2DAngleParameters(
      angleFFTParams = AngleFFTParameters (
        FFTParams = FFTParams.fixed(
          dataWidth = 16,
          twiddleWidth = 16,
          numPoints = 32, //angleFFT,
          useBitReverse  = true,
          runTime = true,
          //trimEnable = true,
          numAddPipes = 1,
          numMulPipes = 1,
          use4Muls = true,
          sdfRadix = "2",
          trimType = Convergent,
          expandLogic = Array.fill(log2Up(32))(0),
          keepMSBorLSB = Array.fill(log2Up(32))(true),
          minSRAMdepth = 128, // memories larger than 64 should be mapped on block ram
          binPoint = 14
        ),
        dspQueueParams = SimpleDspQueueCustomParams(queueDepth = 2*rangeFFT,
                          useSyncReadMem = false,
                          useBlockRam = true),
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
          packetSizeEnd  = 32, //angleFFT,
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
        FFTSize = 32, //angleFFT,
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
    ),
    accParams = AccParams(
      proto    = FixedPoint(16.W, 14.BP),
      protoAcc = FixedPoint(22.W, 14.BP),
      maxNumWindows = 64,
      accDepth = 256*32,
    ),
    magParams = MAGParams(
      protoIn  = FixedPoint(16.W, 14.BP),
      protoOut = FixedPoint(16.W, 14.BP),
      protoLog = Some(FixedPoint(16.W, 8.BP)),
      magType  = MagJPLandSqrMag,
      useLast = false, //true,
      numAddPipes = 1,
      numMulPipes = 1
    ),
    accAddress    = AddressSet(0x50000600 , 0xFF),
    magAddress     = AddressSet(0x50000700, 0xFF)
  )
  val gbemacAddressSet = AddressSet(0x20000000 , 0xFF)
  implicit val p: Parameters = Parameters.empty
  val lazyModule = LazyModule(new FFT2DAngleChainWithGbemac(params, gbemacAddressSet, beatBytes) with FFT2DAngleChainWithGbemacPins)
  chisel3.Driver.execute(Array("--target-dir", "./verilog/FFT2DAngleChainWithGbemac"), ()=> lazyModule.module)
}
