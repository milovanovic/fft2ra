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



class AXI4StreamRxJoiner(val rxNum: Int, val beatBytes : Int) extends LazyModule()(Parameters.empty) {
  
  val inNode = Seq.fill(rxNum)(AXI4StreamSlaveNode(AXI4StreamSlaveParameters()))
  val outNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes * rxNum)))))
  
  lazy val module = new LazyModuleImp(this) {
    
    val ioout = outNode.out(0)._1
    
    //val inputData = (0 until rxNum).map(e => RegInit(0.U((beatBytes*8).W))).toSeq.toArray
    
    for (i <- 0 until rxNum) {
      val (in, _)  = inNode(i).in(0)
      in.ready := ioout.ready
    }
    
    // not parameterized
    
    ioout.valid := inNode(0).in(0)._1.valid
    ioout.bits.last := inNode(0).in(0)._1.bits.last
    //ioout.bits.data := Cat(Cat(inNode(3).in(0)._1.bits.data, inNode(2).in(0)._1.bits.data), Cat(inNode(1).in(0)._1.bits.data, inNode(0).in(0)._1.bits.data))
    //ioout.bits.data := Cat((0 until rxNum).map(e => inNode(rxNum - 1 - e).in(0)._1.bits.data).toSeq)
    ioout.bits.data := Cat((0 until rxNum).map(e => inNode(e).in(0)._1.bits.data).toSeq)
  }
}


trait AXI4StreamRxJoinerPins extends AXI4StreamRxJoiner {
  
    val nIn = rxNum
    val inIO: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
      implicit val valName = ValName(s"inIO_$i")
      val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
        inNode(i) := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
      InModuleBody { in.makeIO() }
    }
    
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    val out = InModuleBody { ioOutNode.makeIO() }
}


object AXI4StreamRxJoinerApp extends App
{ 
  
  val rxNum = 8
  val beatBytes = 4
  
  val lazyDut = LazyModule(new AXI4StreamRxJoiner(rxNum, beatBytes) with AXI4StreamRxJoinerPins)

  //(new ChiselStage).execute(Array("--target-dir", "verilog/PreAngleFFTBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
  chisel3.Driver.execute(Array("--target-dir", "verilog/AXI4StreamRxJoiner"), () => lazyDut.module)
}



