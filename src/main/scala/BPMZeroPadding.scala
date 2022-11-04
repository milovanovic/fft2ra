package rspChain

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._


case class BPMParams(
  maxFFTSize: Int = 1024,
  queueSize: Int = 1024,
  rxNum : Int = 4,
  bpmMode: Boolean = false
) {
}

//class BPMDemodulation[T <: Data : Real : BinaryRepresentation]
abstract class BPMDemodulation [D, U, E, O, B <: Data]
(
  params: BPMParams,
  beatBytes: Int
)extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {
//)extends LazyModule()(Parameters.empty) with HasCSR {

  
  val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  
  val masterNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes*params.rxNum)))))
  
  val streamNode = NodeHandle(slaveNode, masterNode)
  
  lazy val module = new LazyModuleImp(this) {
    
    val ioin = slaveNode.in(0)._1
    val ioout = masterNode.out(0)._1
    
    val log2fftSize = log2Up(params.maxFFTSize + 1)
    
    val fftSize        = RegInit(params.maxFFTSize.U(log2fftSize.W))
    val adcSamples   = RegInit(params.maxFFTSize.U(log2fftSize.W))
    
    val cntOutData = RegInit(0.U(log2fftSize.W))
    //val zeroPaddFlag = Wire(Bool())
    
    val fields = Seq(
      RegField(log2fftSize, fftSize,
        RegFieldDesc(name = "fftSize", desc = "Configured size of the FFT")),
      RegField(log2fftSize, adcSamples,
        RegFieldDesc(name = "adcSamples", desc = "Number of samples per chirp"))
    )
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
    
   /* when (cntOutData === (fftSize - 1.U) && ioout.fire()) {
      cntOutData := 0.U
    }.elsewhen (ioout.fire()) {
      cntOutData := cntOutData + 1.U
    }
    
    when (cntOutData < adcSamples) {
      zeroPaddFlag := false.B
    }.otherwise {
      zeroPaddFlag := true.B
    }*/
    
    if(params.bpmMode == true) {
      
      val queue = Module(new Queue(UInt((beatBytes*params.rxNum*8).W), (params.queueSize >> 2)))
      //val queue2 = Module(new Queue(UInt(128.W), (32)))
      
      val queueLast = Module(new Queue(Bool(), (params.queueSize >> 2)))
      
      val useFirstInputSlot = RegInit(Bool(), true.B)
      val useSecondOutputSlot = RegInit(Bool(), false.B)
      val firstInputSlot = RegInit(UInt((beatBytes*params.rxNum*8).W), 0.U)
      val secondOutputSlot = RegInit(UInt((beatBytes*params.rxNum*8).W), 0.U)
      
      val lastArrived = RegInit(Bool(), false.B)
      
      val sumResult = Wire(UInt((beatBytes*params.rxNum*8).W))
      val subtractionResult = Wire(UInt((beatBytes*params.rxNum*8).W))
      
      ioin.ready := queue.io.enq.ready
      queue.io.enq.valid := ioin.fire()
      queue.io.enq.bits := ioin.bits.data
      //queue.io.deq.ready := ioout.ready || useFirstInputSlot
      queue.io.deq.ready := ioout.ready //(ioout.ready && !useFirstInputSlot) || (useFirstInputSlot && ioout.ready)
      //queue.io.deq.ready := (ioout.ready || useFirstInputSlot) && !zeroPaddFlag
      
      queueLast.io.enq.valid := ioin.fire()
      queueLast.io.enq.bits := ioin.bits.last
      queueLast.io.deq.ready := ioout.ready //|| useFirstInputSlot
      //queueLast.io.deq.ready := (ioout.ready || useFirstInputSlot) && !zeroPaddFlag
      
      val width = (beatBytes >> 1) * 8
      val bp = (beatBytes >> 1) * 8 - 2
      val channels = params.rxNum * 2
      
      //sumResult := Cat((0 until 8).map(e => ((firstInputSlot((7-e)*16+15, (7-e)*16).asTypeOf(UInt(16.W)) +& queue.io.deq.bits((7-e)*16+15, (7-e)*16).asTypeOf(UInt(16.W))) >> 1).asTypeOf(UInt(16.W))).toSeq)
      //subtractionResult := Cat((0 until 8).map(e => ((firstInputSlot((7-e)*16+15, (7-e)*16).asTypeOf(UInt(16.W)) -& queue.io.deq.bits((7-e)*16+15, (7-e)*16).asTypeOf(UInt(16.W))) >> 1).asTypeOf(UInt(16.W))).toSeq)
      sumResult := Cat((0 until channels).map(e => ((firstInputSlot((channels - e) * width - 1, (channels - 1 - e) * width).asTypeOf(FixedPoint(width.W, bp.BP)) +& queue.io.deq.bits((channels - e) * width - 1, (channels - 1 - e) * width).asTypeOf(FixedPoint(width.W, bp.BP))) >> 1).asTypeOf(UInt(width.W))).toSeq)
      subtractionResult := Cat((0 until channels).map(e => ((firstInputSlot((channels - e) * width - 1, (channels - 1 - e) * width).asTypeOf(FixedPoint(width.W, bp.BP)) -& queue.io.deq.bits((channels - e) * width - 1, (channels - 1 - e) * width).asTypeOf(FixedPoint(width.W, bp.BP))) >> 1).asTypeOf(UInt(width.W))).toSeq)
      
      ioout.valid := ioout.ready && ((!useFirstInputSlot && queue.io.deq.ready && queue.io.deq.valid) || useSecondOutputSlot)
      //ioout.valid := ((!useFirstInputSlot && queue.io.deq.ready && queue.io.deq.valid) || useSecondOutputSlot)
      
      //ioout.bits.data := Mux(useSecondOutputSlot, secondOutputSlot, ((firstInputSlot +& queue.io.deq.bits) >> 1).asTypeOf(UInt(16.W)))
      ioout.bits.data := Mux(useSecondOutputSlot, secondOutputSlot, sumResult)
      
      when(queue.io.deq.ready && queue.io.deq.valid) {
        useFirstInputSlot := !useFirstInputSlot
      }
      when(ioout.ready && ioout.valid) {
        useSecondOutputSlot := !useSecondOutputSlot
      }
      
      when(useFirstInputSlot && queue.io.deq.ready && queue.io.deq.valid) {
        firstInputSlot := queue.io.deq.bits
      }
      
      when(!useFirstInputSlot && queue.io.deq.ready && queue.io.deq.valid) {
        //secondOutputSlot := ((firstInputSlot -& queue.io.deq.bits) >> 1).asTypeOf(UInt(16.W))
        secondOutputSlot := subtractionResult
      }
      
      //inOutCounter := inOutCounter +& (queue.io.deq.ready && queue.io.deq.valid) -& (ioout.ready && ioout.valid)
      
      when(queueLast.io.deq.bits && queueLast.io.deq.valid && queueLast.io.deq.ready) {
        lastArrived := true.B
      }.elsewhen(ioout.bits.last && ioout.valid && ioout.ready) {
        lastArrived := false.B
      }
      
      ioout.bits.last := lastArrived && ioout.valid && useSecondOutputSlot
      
      dontTouch(firstInputSlot)
      dontTouch(secondOutputSlot)
      dontTouch(useFirstInputSlot)
      dontTouch(useSecondOutputSlot)
      /*when (zeroPaddFlag) {
      out.valid := true.B
      out.bits.data := 0.U
      in.ready := dataQueue.io.enq.ready
    }
    .otherwise {
      out.valid := dataQueue.io.deq.valid
      out.bits.data := dataQueue.io.deq.bits
      in.ready := dataQueue.io.enq.ready
    }
    
    bits.last?
    
    */
      
    } else {
      
      ioin.ready := ioout.ready
      ioout.valid := ioin.valid
      ioout.bits.data := ioin.bits.data
      ioout.bits.last := ioin.bits.last
    
    }
  
  }
  
}

class AXI4BPMDemodulationBlock(val params: BPMParams, address: AddressSet, val beatBytes: Int = 4)(implicit p: Parameters) extends BPMDemodulation[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

trait BPMStreamPins extends AXI4BPMDemodulationBlock {
  //def beatBytes: Int = 4
  
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes*params.rxNum)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  
  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes*params.rxNum)) := ioInNode
  
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
  
  //slaveNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode
  //ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := masterNode
  
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

object BPMDemodulationApp extends App {

  val beatBytes = 4
  val params: BPMParams = BPMParams(
    maxFFTSize = 1024, 
    queueSize = 1024, 
    rxNum = 8, 
    bpmMode = true
  )
  
  val baseAddress = 0x500
  
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4BPMDemodulationBlock(params, AddressSet(baseAddress, 0xFF), beatBytes) with BPMStreamPins {})

  //(new ChiselStage).execute(Array("--target-dir", "verilog/BPMDemodulation"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
  chisel3.Driver.execute(Array("--target-dir", "verilog/BPMDemodulation"), () => lazyDut.module)

}
