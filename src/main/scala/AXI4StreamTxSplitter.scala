package rspChain

import chisel3._
import chisel3.util._
import chisel3.experimental._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import dsptools.numbers._
import dspblocks._


case class AXI4StreamTxSplitterParameters (
  maxFFTSize : Int = 8192,
  FFTSize: Int = 1024,
  txNum: Int = 4,
  rxNum: Int = 4
) {
}


abstract class AXI4StreamTxSplitter [D, U, E, O, B <: Data] (params: AXI4StreamTxSplitterParameters, beatBytes : Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val masterNodes = (0 until params.txNum).map(e => AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", beatBytes * params.rxNum)))))).toSeq.toArray
  
  val streamNode = NodeHandle(slaveNode, masterNodes(0))
  
  lazy val module = new LazyModuleImp(this) {
    
    val in = slaveNode.in(0)._1
    
    val sampleCounter = RegInit(UInt(log2Up(params.maxFFTSize+2).W), 0.U)
    val txCounter = RegInit(UInt(log2Up(params.txNum+1).W), 0.U)
    
    val log2fftSize = log2Up(params.maxFFTSize + 1)
    val fftSize        = RegInit(params.FFTSize.U(log2fftSize.W))
    
    val fields = Seq(
      RegField(log2fftSize, fftSize,
        RegFieldDesc(name = "fftSize", desc = "Configured size of the FFT"))
    )
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
    
    when(in.fire() && (sampleCounter === (fftSize - 1.U))) {
      sampleCounter := 0.U
    }.elsewhen(in.fire()) {
      sampleCounter := sampleCounter + 1.U
    }
    
    when(in.fire() && (sampleCounter === (fftSize - 1.U)) && (txCounter === (params.txNum - 1).U)) {
      txCounter := 0.U
    }.elsewhen(in.fire() && (sampleCounter === (fftSize - 1.U))) {
      txCounter := txCounter + 1.U
    }
    
    val readies = (0 until (params.txNum)).map(e => masterNodes(e).out(0)._1.ready).toSeq.toArray
    val thresholds = (0 until (params.txNum + 1)).map(e => e).toSeq.toArray
    
    in.ready := readies.foldLeft(true.B)(_ && _)
    
    for (i <- 0 to (params.txNum - 1)) {
      masterNodes(i).out(0)._1.valid := Mux((txCounter >= thresholds(i).U) && (txCounter < thresholds(i+1).U), in.valid, false.B)
      masterNodes(i).out(0)._1.bits.data := Mux((txCounter >= thresholds(i).U) && (txCounter < thresholds(i+1).U), in.bits.data, 0.U)
      masterNodes(i).out(0)._1.bits.last := Mux((txCounter >= thresholds(i).U) && (txCounter < thresholds(i+1).U), in.bits.last, false.B)
      /*masterNodes((params.txNum - 1) - i).out(0)._1.valid := Mux((txCounter >= thresholds(i).U) && (txCounter < thresholds(i+1).U), in.valid, false.B)
      masterNodes((params.txNum - 1) - i).out(0)._1.bits.data := Mux((txCounter >= thresholds(i).U) && (txCounter < thresholds(i+1).U), in.bits.data, 0.U)
      masterNodes((params.txNum - 1) - i).out(0)._1.bits.last := Mux((txCounter >= thresholds(i).U) && (txCounter < thresholds(i+1).U), in.bits.last, false.B)*/
    }
  }
}


class AXI4TxSplitterBlock(val params: AXI4StreamTxSplitterParameters, address: AddressSet, val beatBytes: Int = 4)(implicit p: Parameters) extends AXI4StreamTxSplitter[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}


trait AXI4StreamTxSplitterPins extends AXI4TxSplitterBlock {
    
    val outIOs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until params.txNum) yield {
      implicit val valName = ValName(s"outIOs_$o")
      val out = BundleBridgeSink[AXI4StreamBundle]()
      out := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := masterNodes(o)
      InModuleBody { out.makeIO() }
    }

    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes * params.txNum, i = 0)))
    slaveNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes * params.txNum)) := ioInNode
    val in = InModuleBody { ioInNode.makeIO() }
    
    def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    val ioMem = mem.map { m => {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

      m :=
        BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
        ioMemNode

      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }}
}


object AXI4StreamTxSplitterApp extends App
{ 
  implicit val p: Parameters = Parameters.empty
  
  val params: AXI4StreamTxSplitterParameters = AXI4StreamTxSplitterParameters(
    maxFFTSize = 8192,
    FFTSize = 1024,
    txNum = 4,
    rxNum = 4
  )
  val beatBytes = 4
  val baseAddress = 0x500
  
  val lazyDut = LazyModule(new AXI4TxSplitterBlock(params, AddressSet(baseAddress, 0xFF), beatBytes) with AXI4StreamTxSplitterPins)

  //(new ChiselStage).execute(Array("--target-dir", "verilog/PreAngleFFTBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
  chisel3.Driver.execute(Array("--target-dir", "verilog/AXI4TxSplitterBlock"), () => lazyDut.module)
}



 
