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


class PreAngleFFTTester(   
  dut: PreAngleFFTBlock  with PreAngleFFTBlockPins,
  beatBytes: Int
)  extends PeekPokeTester(dut.module) {

  poke(dut.ins(0).valid, 0)
  poke(dut.ins(0).bits.data, 0)
  poke(dut.ins(0).bits.last, 0)
  poke(dut.ins(1).valid, 0)
  poke(dut.ins(1).bits.data, 0)
  poke(dut.ins(1).bits.last, 0)
  poke(dut.out.ready, 0)
  step(1)
  
  var idx = 0
  var i0 = 0
  var i1 = 1
  
  poke(dut.out.ready, 1)
  
  while(idx < 128) {
    if(peek(dut.ins(0).ready) > 0) {
      poke(dut.ins(0).valid, 1)
      poke(dut.ins(0).bits.data, i0)
      poke(dut.ins(1).valid, 0)
      i0 += 2
      //idx += 1
      //step(1)
    }
    if(peek(dut.ins(1).ready) > 0) {
      poke(dut.ins(1).valid, 1)
      poke(dut.ins(1).bits.data, i1)
      poke(dut.ins(0).valid, 0)
      i1 +=2
      //idx += 1
      //step(1)
    }
    if((peek(dut.ins(0).ready) > 0) || (peek(dut.ins(1).ready) > 0)) {
      idx += 1
      step(1)
    }
  }
  
  idx = 0
  
  while(idx < 128) {
    if(peek(dut.ins(0).ready) > 0) {
      poke(dut.ins(0).valid, 1)
      poke(dut.ins(0).bits.data, i0)
      poke(dut.ins(1).valid, 1)
      i0 += 2
      //idx += 1
      //step(1)
    }
    if(peek(dut.ins(1).ready) > 0) {
      poke(dut.ins(1).valid, 1)
      poke(dut.ins(1).bits.data, i1)
      poke(dut.ins(0).valid, 1)
      i1 +=2
      //idx += 1
      //step(1)
    }
    if((peek(dut.ins(0).ready) > 0) || (peek(dut.ins(1).ready) > 0)) {
      idx += 1
      step(1)
    }
  }
  
  step(16)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  
  poke(dut.out.ready, 0)
  step(2)
  poke(dut.out.ready, 1)
  step(2)
  poke(dut.out.ready, 0)
  step(2)
  poke(dut.out.ready, 1)
  step(2)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(2)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(2)
  poke(dut.out.ready, 0)
  step(2)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(2)
  poke(dut.out.ready, 1)
  step(1)

}


class PreAngleFFTSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 4
  val params: PreAngleFFTBlockParameters = PreAngleFFTBlockParameters(
    maxFFTSize = 1024, 
    rxNum = 4, 
    bpmMode = false
  )
  
  it should "Test PreAngleFFTBlock" in {
    val lazyDut = LazyModule(new PreAngleFFTBlock(params, beatBytes) with PreAngleFFTBlockPins {
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new PreAngleFFTTester(lazyDut, beatBytes)
    } should be (true)
  }
}
