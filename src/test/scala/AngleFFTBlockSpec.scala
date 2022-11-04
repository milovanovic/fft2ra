package rspChain

import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import scala.io.Source
import java.io._
import scala.math.{pow, sqrt, sin, cos}
import breeze.plot._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}

import fft._
import dsputils._
import zeropadder._


class AngleFFTBlockTester(
  dut: AngleFFT with AngleFFTPins,
  beatBytes: Int
)  extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel {

  def memAXI: AXI4Bundle = dut.ioMem.get
  
  val returnVal = new Array[BigInt](1024*8)
  
  poke(dut.ins(1).valid, 0)
  poke(dut.ins(1).bits.data, 0)
  poke(dut.ins(1).bits.last, 0)
  poke(dut.ins(0).valid, 0)
  poke(dut.ins(0).bits.data, 0)
  poke(dut.ins(0).bits.last, 0)
  poke(dut.out.ready, 0)
  
  step(1)
  
  val size2 = 8.0
  
  var ii = 0
  var i = 0
  
  //val fstreaminput = new PrintWriter(new FileWriter("input.txt"));
  
  while(ii < 64) {
    poke(dut.ins(0).valid, 1)
    poke(dut.ins(1).valid, 1)
    poke(dut.out.ready, 1)
    val x = (ii % 4).toInt //.toDouble
    val m = scala.math.cos(2.0 * scala.math.Pi * x.toDouble * 1.0 / size2)
    val mm = {
      if (m >= 0.0) {
        (pow(2, 13).toDouble * m).toInt
      } else {
        (pow(2, 13).toDouble * m).toInt + pow(2, 16).toInt
      }
    }
    val n = scala.math.cos(2.0 * scala.math.Pi * (x.toDouble + 4) * 1.0 / size2)
    val nn = {
      if (n >= 0.0) {
        (pow(2, 13).toDouble * n).toInt
      } else {
        (pow(2, 13).toDouble * n).toInt + pow(2, 16).toInt
      }
    }
    
    //fstreaminput.println(inputData.toInt)
    poke(dut.ins(0).bits.data, mm.toInt)
    poke(dut.ins(1).bits.data, nn.toInt)

    
    if (ii == (64 - 1)) {
      poke(dut.ins(0).bits.last, 1)
      poke(dut.ins(1).bits.last, 1)
    }
    
    if(peek(dut.out.valid) == 1) {
      returnVal(i) = peek(dut.out.bits.data).toInt
      i += 1
    }
    
    ii +=1
    step(1)
  }
  
  poke(dut.ins(0).valid, 0)
  poke(dut.ins(0).bits.last, 0)
  poke(dut.ins(1).valid, 0)
  poke(dut.ins(1).bits.last, 0)

  
  //fstreaminput.flush();
  
  //step(35000)
  
  while (i < 64*2*4) {
    if(peek(dut.out.valid) == 1) {
      returnVal(i) = peek(dut.out.bits.data)//.toInt
      i += 1
    }
    step(1)
  }
  step(200)
  
  /*val fstream = new PrintWriter(new FileWriter("log.txt"));
  
  i = 0
  while (i < 64) {
    fstream.println(returnVal(i))
    i += 1
  }
  fstream.flush();*/
}


class AngleFFTBlockSpec extends FlatSpec with Matchers {

  val beatBytes = 4
  val angleFFT = 8 // 64
  val rangeFFT = 1024
  val bpmMode = false
  val rxNum = 4
  val txNum = 2
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

  val testModule = LazyModule(new AngleFFT(params, beatBytes) with AngleFFTPins)
  it should "Test Angle FFT" in {
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => testModule.module) {
      c => new AngleFFTBlockTester(dut = testModule, beatBytes = beatBytes) //, params = paramsFFT2D)
    } should be (true)
  }
}
