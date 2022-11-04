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


class FileMemoryModule(val bpmMode : Boolean, val beatBytes : Int) extends LazyModule()(Parameters.empty) {
  
  /*val outNode0 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes)))))
  val outNode1 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes)))))
  val outNode2 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes)))))
  val outNode3 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes)))))*/
  
  val streamNodes = (0 until 4).map(e => AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters(s"out_$e", n = beatBytes)))))).toSeq.toArray
  
  
  lazy val module = new LazyModuleImp(this) {
    
    val ioout0 = streamNodes(0).out(0)._1
    val ioout1 = streamNodes(1).out(0)._1
    val ioout2 = streamNodes(2).out(0)._1
    val ioout3 = streamNodes(3).out(0)._1
    
    val MemData = Wire(UInt(128.W))
    val address = RegInit(UInt(14.W), 0.U)
    //when(ioout0.valid && ioout0.ready && (address < 2048.U)) {
    when(ioout0.ready && (address < 2048.U)) {
      address := address + 1.U
    }
    
    val memory = Mem(4096, UInt(128.W))
    MemData := memory(address)
    if (!bpmMode)
      loadMemoryFromFile(memory, "/home/vukand/Desktop/playground/2DFFT/ja/python/radar_data_128.txt")
    else
      loadMemoryFromFile(memory, "/home/vukand/Desktop/playground/2DFFT/ja/python/radar_data_128_bpm.txt")
    
    val outData0 = RegInit(UInt(32.W), 0.U)
    val outData1 = RegInit(UInt(32.W), 0.U)
    val outData2 = RegInit(UInt(32.W), 0.U)
    val outData3 = RegInit(UInt(32.W), 0.U)
    
    val outValid0 = RegInit(Bool(), false.B)
    val outValid1 = RegInit(Bool(), false.B)
    val outValid2 = RegInit(Bool(), false.B)
    val outValid3 = RegInit(Bool(), false.B)
    
    outData0 := MemData(127, 96)
    outData1 := MemData(95, 64)
    outData2 := MemData(63, 32)
    outData3 := MemData(31, 0)
    
    ioout0.bits.data := outData0
    ioout1.bits.data := outData1
    ioout2.bits.data := outData2
    ioout3.bits.data := outData3
    
    outValid0 := ioout0.ready && (address < 2048.U)
    outValid1 := ioout1.ready && (address < 2048.U)
    outValid2 := ioout2.ready && (address < 2048.U)
    outValid3 := ioout3.ready && (address < 2048.U)
    
    ioout0.valid := outValid0
    ioout1.valid := outValid1
    ioout2.valid := outValid2
    ioout3.valid := outValid3
    
    ioout0.bits.last := false.B
    ioout1.bits.last := false.B
    ioout2.bits.last := false.B
    ioout3.bits.last := false.B
    
  }
}


trait FileMemoryModulePins extends FileMemoryModule {
    
    val ioOutNode0 = BundleBridgeSink[AXI4StreamBundle]()
    //ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    ioOutNode0 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNodes(0)
    val out0 = InModuleBody { ioOutNode0.makeIO() }
    
    val ioOutNode1 = BundleBridgeSink[AXI4StreamBundle]()
    //ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    ioOutNode1 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNodes(1)
    val out1 = InModuleBody { ioOutNode1.makeIO() }
    
    val ioOutNode2 = BundleBridgeSink[AXI4StreamBundle]()
    //ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    ioOutNode2 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNodes(2)
    val out2 = InModuleBody { ioOutNode2.makeIO() }
    
    val ioOutNode3 = BundleBridgeSink[AXI4StreamBundle]()
    //ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    ioOutNode3 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNodes(3)
    val out3 = InModuleBody { ioOutNode3.makeIO() }
}


object FileMemoryModuleApp extends App
{ 
  
  val bpmMode = false
  val beatBytes = 4
  
  val lazyDut = LazyModule(new FileMemoryModule(bpmMode, beatBytes) with FileMemoryModulePins)

  //(new ChiselStage).execute(Array("--target-dir", "verilog/PreAngleFFTBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
  chisel3.Driver.execute(Array("--target-dir", "verilog/FileMemoryModule"), () => lazyDut.module)
}



