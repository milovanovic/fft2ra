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


class FileMemoryModuleJoint(val bpmMode : Boolean, val beatBytes : Int) extends LazyModule()(Parameters.empty) {
  
  //val inNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val outNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes*4)))))
  
  lazy val module = new LazyModuleImp(this) {
    
    val ioout = outNode.out(0)._1
    
    val MemData = Wire(UInt(128.W))
    val address = RegInit(UInt(14.W), 0.U)
    //when(ioout0.valid && ioout0.ready && (address < 2048.U)) {
    when(ioout.ready && (address < 2048.U)) {
      address := address + 1.U
    }
    
    val memory = Mem(4096, UInt(128.W))
    MemData := memory(address)
    if (!bpmMode)
      loadMemoryFromFile(memory, "/home/vukand/Desktop/playground/2DFFT/ja/python/radar_data_128.txt")
    else
      loadMemoryFromFile(memory, "/home/vukand/Desktop/playground/2DFFT/ja/python/radar_data_128_bpm.txt")
    
    val outData = RegInit(UInt(128.W), 0.U)
    
    val outValid = RegInit(Bool(), false.B)
    
    outData := MemData(127, 0)
    
    ioout.bits.data := outData
    
    outValid := ioout.ready && (address < 2048.U)
    
    ioout.valid := outValid
    
    ioout.bits.last := false.B
    
  }
}


trait FileMemoryModuleJointPins extends FileMemoryModuleJoint {
    
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
    //ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode
    val out = InModuleBody { ioOutNode.makeIO() }

}


object FileMemoryModuleJointApp extends App
{ 
  
  val bpmMode = false
  val beatBytes = 4
  
  val lazyDut = LazyModule(new FileMemoryModuleJoint(bpmMode, beatBytes) with FileMemoryModuleJointPins)

  //(new ChiselStage).execute(Array("--target-dir", "verilog/PreAngleFFTBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
  chisel3.Driver.execute(Array("--target-dir", "verilog/FileMemoryModuleJoint"), () => lazyDut.module)
}



