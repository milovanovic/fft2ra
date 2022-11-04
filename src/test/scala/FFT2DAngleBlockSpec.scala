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
import xWRDataPreProc._


class FFT2DAngleBlockTester(
  dut: FFT2DAngleBlock with FFT2DAngleBlockPins,
  beatBytes: Int,
  rangeFFT: Int,
  rxNum: Int,
  txNum: Int,
  bpmMode: Boolean
)  extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel {

  def memAXI: AXI4Bundle = dut.ioMem.get
  
  //val rangeFFT = 1024
  //val rxNum = 4
  //val txNum = 4
  val repeatNum = 2
  val totalNum = rangeFFT * txNum * rxNum
  
  val returnVal = new Array[BigInt](totalNum)
  val returnVal2 = new Array[BigInt](totalNum)
  
  poke(dut.ins(0).valid, 0)
  poke(dut.ins(0).bits.data, 0)
  poke(dut.ins(0).bits.last, 0)
  poke(dut.ins(1).valid, 0)
  poke(dut.ins(1).bits.data, 0)
  poke(dut.ins(1).bits.last, 0)
  poke(dut.ins(2).valid, 0)
  poke(dut.ins(2).bits.data, 0)
  poke(dut.ins(2).bits.last, 0)
  poke(dut.ins(3).valid, 0)
  poke(dut.ins(3).bits.data, 0)
  poke(dut.ins(3).bits.last, 0)
  poke(dut.out.ready, 0)
  
  step(1)
  
  val size1 = rangeFFT //1024.0
  val size2 = txNum * rxNum //8.0
  
  var ii = 0
  var i = 0
  
  //val fstreaminput = new PrintWriter(new FileWriter("input.txt"));
  
  if(bpmMode == false) {
    //while(ii < 1024*2) {
    while(ii < rangeFFT*txNum*repeatNum) {
      poke(dut.ins(0).valid, 1)
      poke(dut.ins(1).valid, 1)
      poke(dut.ins(2).valid, 1)
      poke(dut.ins(3).valid, 1)
      poke(dut.out.ready, 1)
      val x = (ii % size1).toInt //.toDouble
      val y = (ii / size1).toInt //.toDouble
      val m1 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 4 + 0) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m2 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 4 + 1) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m3 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 4 + 2) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m4 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 4 + 3) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val mm1 = {
        if (m1 >= 0.0) {
          (pow(2, 13).toDouble * m1).toInt
        } else {
          (pow(2, 13).toDouble * m1).toInt + pow(2, 16).toInt
        }
      }
      val mm2 = {
        if (m2 >= 0.0) {
          (pow(2, 13).toDouble * m2).toInt
        } else {
          (pow(2, 13).toDouble * m2).toInt + pow(2, 16).toInt
        }
      }
      val mm3 = {
        if (m3 >= 0.0) {
          (pow(2, 13).toDouble * m3).toInt
        } else {
          (pow(2, 13).toDouble * m3).toInt + pow(2, 16).toInt
        }
      }
      val mm4 = {
        if (m4 >= 0.0) {
          (pow(2, 13).toDouble * m4).toInt
        } else {
          (pow(2, 13).toDouble * m4).toInt + pow(2, 16).toInt
        }
      }
      val n1 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 4 + 0) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n2 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 4 + 1) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n3 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 4 + 2) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n4 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 4 + 3) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val nn1 = {
        if (n1 >= 0.0) {
          (pow(2, 13).toDouble * n1).toInt
        } else {
          (pow(2, 13).toDouble * n1).toInt + pow(2, 16).toInt
        }
      }
      val nn2 = {
        if (n2 >= 0.0) {
          (pow(2, 13).toDouble * n2).toInt
        } else {
          (pow(2, 13).toDouble * n2).toInt + pow(2, 16).toInt
        }
      }
      val nn3 = {
        if (n3 >= 0.0) {
          (pow(2, 13).toDouble * n3).toInt
        } else {
          (pow(2, 13).toDouble * n3).toInt + pow(2, 16).toInt
        }
      }
      val nn4 = {
        if (n4 >= 0.0) {
          (pow(2, 13).toDouble * n4).toInt
        } else {
          (pow(2, 13).toDouble * n4).toInt + pow(2, 16).toInt
        }
      }
      val inputData1 = (mm1 + nn1) % pow(2, 16).toInt
      val inputData2 = (mm2 + nn2) % pow(2, 16).toInt
      val inputData3 = (mm3 + nn3) % pow(2, 16).toInt
      val inputData4 = (mm4 + nn4) % pow(2, 16).toInt
      //fstreaminput.println(inputData.toInt)
      poke(dut.ins(0).bits.data, inputData1.toInt)
      poke(dut.ins(1).bits.data, inputData2.toInt)
      poke(dut.ins(2).bits.data, inputData3.toInt)
      poke(dut.ins(3).bits.data, inputData4.toInt)
      
      if (ii == (rangeFFT*txNum - 1)) {
        poke(dut.ins(0).bits.last, 1)
        poke(dut.ins(1).bits.last, 1)
        poke(dut.ins(2).bits.last, 1)
        poke(dut.ins(3).bits.last, 1)
      }
      
      if(peek(dut.out.valid) == 1) {
        returnVal(i) = peek(dut.out.bits.data).toInt
        i += 1
      }
      
      if(peek(dut.ins(0).ready) == 1) {
        ii +=1
      }
      step(1)
    }
  }
  else {
    while(ii < rangeFFT*txNum*repeatNum) {
      poke(dut.ins(0).valid, 1)
      poke(dut.ins(1).valid, 1)
      poke(dut.ins(2).valid, 1)
      poke(dut.ins(3).valid, 1)
      poke(dut.out.ready, 1)
      val x = (ii % size1).toInt //.toDouble
      val y = (ii / size1).toInt //.toDouble
      val m1 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 0) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m2 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 1) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m3 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 2) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m4 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 3) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m5 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 4) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m6 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 5) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m7 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 6) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val m8 = scala.math.cos(2.0 * scala.math.Pi * (y.toDouble * 8 + 7) * 1.0 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 3 / size1)
      val mm1 = {
        if (m1 >= 0.0) {
          (pow(2, 12).toDouble * m1).toInt
        } else {
          (pow(2, 12).toDouble * m1).toInt + pow(2, 16).toInt
        }
      }
      val mm2 = {
        if (m2 >= 0.0) {
          (pow(2, 12).toDouble * m2).toInt
        } else {
          (pow(2, 12).toDouble * m2).toInt + pow(2, 16).toInt
        }
      }
      val mm3 = {
        if (m3 >= 0.0) {
          (pow(2, 12).toDouble * m3).toInt
        } else {
          (pow(2, 12).toDouble * m3).toInt + pow(2, 16).toInt
        }
      }
      val mm4 = {
        if (m4 >= 0.0) {
          (pow(2, 12).toDouble * m4).toInt
        } else {
          (pow(2, 12).toDouble * m4).toInt + pow(2, 16).toInt
        }
      }
      val mm5 = {
        if (m5 >= 0.0) {
          (pow(2, 12).toDouble * m5).toInt
        } else {
          (pow(2, 12).toDouble * m5).toInt + pow(2, 16).toInt
        }
      }
      val mm6 = {
        if (m6 >= 0.0) {
          (pow(2, 12).toDouble * m6).toInt
        } else {
          (pow(2, 12).toDouble * m6).toInt + pow(2, 16).toInt
        }
      }
      val mm7 = {
        if (m7 >= 0.0) {
          (pow(2, 12).toDouble * m7).toInt
        } else {
          (pow(2, 12).toDouble * m7).toInt + pow(2, 16).toInt
        }
      }
      val mm8 = {
        if (m8 >= 0.0) {
          (pow(2, 12).toDouble * m8).toInt
        } else {
          (pow(2, 12).toDouble * m8).toInt + pow(2, 16).toInt
        }
      }
      val n1 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 0) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n2 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 1) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n3 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 2) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n4 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 3) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n5 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 4) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n6 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 5) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n7 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 6) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val n8 = scala.math.cos(2 * scala.math.Pi * (y.toDouble * 8 + 7) * 2 / size2) * scala.math.cos(2 * scala.math.Pi * x.toDouble * 8 / size1)
      val nn1 = {
        if (n1 >= 0.0) {
          (pow(2, 12).toDouble * n1).toInt
        } else {
          (pow(2, 12).toDouble * n1).toInt + pow(2, 16).toInt
        }
      }
      val nn2 = {
        if (n2 >= 0.0) {
          (pow(2, 12).toDouble * n2).toInt
        } else {
          (pow(2, 12).toDouble * n2).toInt + pow(2, 16).toInt
        }
      }
      val nn3 = {
        if (n3 >= 0.0) {
          (pow(2, 12).toDouble * n3).toInt
        } else {
          (pow(2, 12).toDouble * n3).toInt + pow(2, 16).toInt
        }
      }
      val nn4 = {
        if (n4 >= 0.0) {
          (pow(2, 12).toDouble * n4).toInt
        } else {
          (pow(2, 12).toDouble * n4).toInt + pow(2, 16).toInt
        }
      }
      val nn5 = {
        if (n5 >= 0.0) {
          (pow(2, 12).toDouble * n5).toInt
        } else {
          (pow(2, 12).toDouble * n5).toInt + pow(2, 16).toInt
        }
      }
      val nn6 = {
        if (n6 >= 0.0) {
          (pow(2, 12).toDouble * n6).toInt
        } else {
          (pow(2, 12).toDouble * n6).toInt + pow(2, 16).toInt
        }
      }
      val nn7 = {
        if (n7 >= 0.0) {
          (pow(2, 12).toDouble * n7).toInt
        } else {
          (pow(2, 12).toDouble * n7).toInt + pow(2, 16).toInt
        }
      }
      val nn8 = {
        if (n8 >= 0.0) {
          (pow(2, 12).toDouble * n8).toInt
        } else {
          (pow(2, 12).toDouble * n8).toInt + pow(2, 16).toInt
        }
      }
      
      val inputData1 = if((ii % rangeFFT*2) < rangeFFT) (mm1 + mm5 + nn1 + nn5) % pow(2, 16).toInt else (mm1 - mm5 + nn1 - nn5) % pow(2, 16).toInt
      val inputData2 = if((ii % rangeFFT*2) < rangeFFT) (mm2 + mm6 + nn2 + nn6) % pow(2, 16).toInt else (mm2 - mm6 + nn2 - nn6) % pow(2, 16).toInt
      val inputData3 = if((ii % rangeFFT*2) < rangeFFT) (mm3 + mm7 + nn3 + nn7) % pow(2, 16).toInt else (mm3 - mm7 + nn3 - nn7) % pow(2, 16).toInt
      val inputData4 = if((ii % rangeFFT*2) < rangeFFT) (mm4 + mm8 + nn4 + nn8) % pow(2, 16).toInt else (mm4 - mm8 + nn4 - nn8) % pow(2, 16).toInt

      //fstreaminput.println(inputData.toInt)
      poke(dut.ins(0).bits.data, inputData1.toInt)
      poke(dut.ins(1).bits.data, inputData2.toInt)
      poke(dut.ins(2).bits.data, inputData3.toInt)
      poke(dut.ins(3).bits.data, inputData4.toInt)
      
      if (ii == (rangeFFT*txNum - 1)) {
        poke(dut.ins(0).bits.last, 1)
        poke(dut.ins(1).bits.last, 1)
        poke(dut.ins(2).bits.last, 1)
        poke(dut.ins(3).bits.last, 1)
      }
      
      if(peek(dut.out.valid) == 1) {
        returnVal(i) = peek(dut.out.bits.data).toInt
        i += 1
      }
      
      if(peek(dut.ins(0).ready) == 1) {
        ii +=1
      }
      step(1)
    }
  }
  
  poke(dut.ins(0).valid, 0)
  poke(dut.ins(0).bits.last, 0)
  poke(dut.ins(1).valid, 0)
  poke(dut.ins(1).bits.last, 0)
  poke(dut.ins(2).valid, 0)
  poke(dut.ins(2).bits.last, 0)
  poke(dut.ins(3).valid, 0)
  poke(dut.ins(3).bits.last, 0)
  
  //fstreaminput.flush();
  
  //step(35000)
  
  while (i < totalNum) {
  //while (i < 8184) {
    if(peek(dut.out.valid) == 1) {
      returnVal(i) = peek(dut.out.bits.data)//.toInt
      i += 1
    }
    step(1)
  }
  while ((i >= totalNum) && (i < totalNum*repeatNum)) {
    if(peek(dut.out.valid) == 1) {
      returnVal2(i%totalNum) = peek(dut.out.bits.data)//.toInt
      i += 1
    }
    step(1)
  }
  step(5)
  
  val fstream = new PrintWriter(new FileWriter("log.txt"))
  
  i = 0
  while (i < totalNum) {
    //fstream.println(returnVal(i))
    fstream.println(returnVal2(i))
    i += 1
  }
  fstream.flush();
}


class FFT2DAngleBlockSpec extends FlatSpec with Matchers {

  val beatBytes = 4
  val rxNum = 4
  val txNum = 2
  val angleFFT = rxNum * txNum // 8 // 64
  val rangeFFT = 1024
  val bpmMode = false
  val totalData = rangeFFT * angleFFT
  val params = FFT2DAngleParameters (
    angleFFTParams = AngleFFTParameters (
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
      dspQueueParams = SimpleDspQueueCustomParams(queueDepth = 2*rangeFFT,
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
    dspQueueParams = SimpleDspQueueCustomParams(queueDepth = 256,
                        useSyncReadMem = false,
                        useBlockRam = false),
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
  )

  val testModule = LazyModule(new FFT2DAngleBlock(params, beatBytes) with FFT2DAngleBlockPins)
  it should "Test 2D Angle FFT" in {
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => testModule.module) {
      c => new FFT2DAngleBlockTester(dut = testModule, beatBytes = beatBytes, rangeFFT = rangeFFT, rxNum = rxNum, txNum = txNum, bpmMode = bpmMode) //, params = paramsFFT2D)
    } should be (true)
  }
}
