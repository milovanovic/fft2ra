package rspChain

import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import scala.io.Source
import java.io._
import scala.math.{pow, sqrt}
import breeze.plot._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}


class BPMTester(   
  dut: AXI4BPMDemodulationBlock  with BPMStreamPins,
  csrAddress: AddressSet,
  beatBytes: Int
)  extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel {
  
  def memAXI: AXI4Bundle = dut.ioMem.get
  
  poke(dut.in.valid, 0)
  poke(dut.in.bits.data, 0)
  poke(dut.in.bits.last, 0)
  poke(dut.out.ready, 0)
  
  step(1)
  
  //poke(dut.reset, 1)
  //step(1)
  //poke(dut.reset, 0)
  
  var idx = 0
  var ii = 64
  
  while(ii > 0) {
    poke(dut.in.valid, 1)
    poke(dut.out.ready, 1)
    poke(dut.in.bits.data, ii)
    ii -=1
    step(1)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.in.valid, 1)
    poke(dut.in.bits.data, ii)
    step(1)
    poke(dut.in.valid, 0)
    ii -=2
    step(1)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.in.valid, 1)
    poke(dut.in.bits.data, ii)
    step(1)
    ii -=2
    poke(dut.in.bits.data, ii)
    step(1)
    poke(dut.in.valid, 0)
    ii -=2
    step(2)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.in.valid, 1)
    poke(dut.in.bits.data, ii)
    step(1)
    poke(dut.in.valid, 0)
    ii -=2
    step(2)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.in.valid, 1)
    poke(dut.in.bits.data, ii)
    step(1)
    ii -=2
    poke(dut.in.bits.data, ii)
    step(1)
    poke(dut.in.valid, 0)
    ii -=2
    step(1)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.in.valid, 1)
    poke(dut.out.ready, 1)
    poke(dut.in.bits.data, ii)
    step(1)
    poke(dut.out.ready, 0)
    ii -=2
    step(1)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.out.ready, 1)
    poke(dut.in.bits.data, ii)
    step(2)
    poke(dut.out.ready, 0)
    ii -=2
    step(2)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.out.ready, 1)
    poke(dut.in.bits.data, ii)
    step(1)
    poke(dut.out.ready, 0)
    ii -=2
    step(2)
  }
  
  ii = 64
  while(ii > 0){
    poke(dut.out.ready, 1)
    poke(dut.in.bits.data, ii)
    step(2)
    poke(dut.out.ready, 0)
    ii -=2
    step(1)
  }
  
  poke(dut.out.ready, 1)
  poke(dut.in.bits.data, 2)
  poke(dut.in.valid, 1)
  poke(dut.in.bits.last, 1)
  step(1)
  poke(dut.in.bits.last, 0)
  poke(dut.in.bits.data, 6)
  step(1)
  poke(dut.in.bits.last, 1)
  poke(dut.in.bits.data, 4)
  step(1)
  poke(dut.in.bits.last, 0)
  poke(dut.in.valid, 0)
  step(1)
  poke(dut.in.bits.last, 1)
  step(1)
  poke(dut.in.bits.last, 0)
  step(1)
  poke(dut.in.bits.last, 1)
  poke(dut.in.valid, 1)
  step(2)
  poke(dut.in.bits.last, 0)
  step(2)
  poke(dut.in.bits.last, 1)
  step(1)
  poke(dut.in.valid, 0)
  step(1)
  poke(dut.in.valid, 1)
  step(1)
  poke(dut.in.bits.last, 0)
  step(500)

}


class BPMSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 4
  val params: BPMParams = BPMParams(
    maxFFTSize = 1024, 
    queueSize = 1024, 
    rxNum = 4, 
    bpmMode = true
  )
  
  it should "Test BPM Demodulation" in {
    val lazyDut = LazyModule(new AXI4BPMDemodulationBlock(params, AddressSet(0x5000, 0xFF), beatBytes) with BPMStreamPins {
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new BPMTester(lazyDut, AddressSet(0x5000, 0xFF), beatBytes)
    } should be (true)
  }
}
