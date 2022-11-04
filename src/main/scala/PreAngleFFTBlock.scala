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


case class PreAngleFFTBlockParameters (
  maxFFTSize : Int = 1024,
  rxNum : Int = 4,
  txNum : Int = 2,
  bpmMode : Boolean = false
) {
  //require(isPow2(rxNum), "fft size must be a power of 2")
}


class PreAngleFFTBlock(val params: PreAngleFFTBlockParameters, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  
  //val inNode1 = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  //val inNode2 = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  //val outNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes * params.rxNum)))))
  
  if (params.bpmMode) require(params.txNum == 2)
  
  val slaveNodes = (0 until params.txNum).map(e => AXI4StreamSlaveNode(AXI4StreamSlaveParameters())).toSeq.toArray
  val masterNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", beatBytes * params.rxNum)))))
  
  /*val parameters = (0 until params.txNum).map(e => AXI4StreamSlaveParameters()).toSeq
  val streamNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) =>
      AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes * params.rxNum))),
    slaveFn = ss => {
      AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())
    }
  )*/
  
  val switchingNum = 1 //params.rxNum //if(params.bpmMode) 1 else params.rxNum
  
  lazy val module = new LazyModuleImp(this) {
    
    //val ioin1 = inNode1.in(0)._1
    //val ioin2 = inNode2.in(0)._1
    //val ioout = outNode.out(0)._1
    //val (ins, _) = streamNode.in.unzip
    //val (outs, _) = streamNode.out.unzip
    val out = masterNode.out(0)._1
    
    val sampleCounter = RegInit(UInt(log2Up(params.maxFFTSize+1).W), 0.U)
    //val threshold = switchingNum.U
    
    /*when(ioin2.fire() && (sampleCounter === (switchingNum * 2 - 1).U)) {
      sampleCounter := 0.U
    }.elsewhen((ioin1.fire() && (sampleCounter < threshold)) || (ioin2.fire() && (sampleCounter >= threshold))) {
      sampleCounter := sampleCounter + 1.U
    }
  
    ioin1.ready := ioout.ready && (sampleCounter < threshold)
    ioin2.ready := ioout.ready && (sampleCounter >= threshold)
    
    ioout.valid := Mux(sampleCounter < threshold, ioin1.valid, ioin2.valid)
    ioout.bits.data := Mux(sampleCounter < threshold, ioin1.bits.data, ioin2.bits.data)
    ioout.bits.last := Mux(sampleCounter < threshold, ioin1.bits.last, ioin2.bits.last)
    */
    
    val fires = (0 until (params.txNum)).map(e => slaveNodes(e).in(0)._1.fire()).toSeq.toArray
    val thresholds = (0 until (params.txNum + 1)).map(e => e.U).toSeq.toArray
    val conditions1 = (0 until (params.txNum)).map(e => fires(e) && (sampleCounter >= thresholds(e)) && (sampleCounter < thresholds(e+1))).toSeq.toArray
    val conditions2 = (0 until (params.txNum)).map(e => (sampleCounter >= thresholds(e)) && (sampleCounter < thresholds(e+1))).toSeq
    val muxCaseValids = (0 until (params.txNum)).map(e => slaveNodes(e).in(0)._1.valid).toSeq
    val muxCaseDatas = (0 until (params.txNum)).map(e => slaveNodes(e).in(0)._1.bits.data).toSeq
    val muxCaseLasts = (0 until (params.txNum)).map(e => slaveNodes(e).in(0)._1.bits.last).toSeq
    
    val valids: Seq[(Bool, Bool)] = conditions2.zip(muxCaseValids)
    val datas: Seq[(Bool, UInt)] = conditions2.zip(muxCaseDatas)
    val lasts: Seq[(Bool, Bool)] = conditions2.zip(muxCaseLasts)
    //val muxCaseValids = (0 until (params.txNum)).map(e => ((sampleCounter >= thresholds(e)) && (sampleCounter < thresholds(e+1)) -> ins(e).valid)).toSeq
    //val muxCaseDatas = (0 until (params.txNum)).map(e => ((sampleCounter >= thresholds(e)) && (sampleCounter < thresholds(e+1)) -> ins(e).bits.data)).toSeq
    //val muxCaseLasts = (0 until (params.txNum)).map(e => ((sampleCounter >= thresholds(e)) && (sampleCounter < thresholds(e+1)) -> ins(e).bits.last)).toSeq
    
    when(fires(params.txNum - 1) && (sampleCounter === (thresholds(params.txNum) - 1.U))) {
      sampleCounter := 0.U
    }.elsewhen(conditions1.foldLeft(false.B)(_ || _)) {
      sampleCounter := sampleCounter + 1.U
    }
    
    for (i <- 0 to (params.txNum - 1)) {
      slaveNodes(i).in(0)._1.ready := out.ready && (sampleCounter >= thresholds(i)) && (sampleCounter < thresholds(i+1))
    }
    
    //outs(0).valid := MuxCase(false.B, muxCaseValids)
    //outs(0).bits.data := MuxCase(0.U, muxCaseDatas)
    //outs(0).bits.last := MuxCase(false.B, muxCaseLasts)
    
    out.valid := MuxCase(false.B, valids)
    out.bits.data := MuxCase(0.U, datas)
    out.bits.last := MuxCase(false.B, lasts)
  
  }
}


trait PreAngleFFTBlockPins extends PreAngleFFTBlock {

    /*val ioInNode1 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes * params.rxNum)))
    inNode1 := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes * params.rxNum)) := ioInNode1
    val in0 = InModuleBody { ioInNode1.makeIO() }
    
    val ioInNode2 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes * params.rxNum)))
    inNode2 := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes * params.rxNum)) := ioInNode2
    val in1 = InModuleBody { ioInNode2.makeIO() }
    
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    val out = InModuleBody { ioOutNode.makeIO() }*/
    
    val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until params.txNum) yield {
      implicit val valName = ValName(s"inIOs_$i")
      val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes * params.rxNum)))
      slaveNodes(i) :=
        BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes * params.rxNum))) :=
        in
      InModuleBody { in.makeIO() }
    }

    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := masterNode
    val out = InModuleBody { ioOutNode.makeIO() }
    
}


object PreAngleFFTBlockApp extends App
{ 
  
  val beatBytes = 4
  val params: PreAngleFFTBlockParameters = PreAngleFFTBlockParameters(
    maxFFTSize = 1024, 
    rxNum = 4,
    txNum = 4,
    bpmMode = false
  )
  
  val lazyDut = LazyModule(new PreAngleFFTBlock(params, beatBytes) with PreAngleFFTBlockPins)

  //(new ChiselStage).execute(Array("--target-dir", "verilog/PreAngleFFTBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
  chisel3.Driver.execute(Array("--target-dir", "verilog/PreAngleFFTBlock"), () => lazyDut.module)
}



