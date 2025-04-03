package ArithmeticUnit

import chisel3._
import chisel3.util._

object CmdList {
  def Addition = 0.U
  def Subtraction = 1.U
  def Multiplication = 2.U
  def Division = 3.U

  def CmdCount = 4
  def CmdLen = log2Ceil(CmdList.CmdCount)
}

case class AUParams(
    width: Int = 32,
    trace: Boolean = false
)

class CmdBundle(implicit p: AUParams) extends Bundle {
  val valid = Input(Bool())
  val op1 = Input(SInt(p.width.W))
  val op2 = Input(SInt(p.width.W))
  val out = Output(SInt(p.width.W))
  val ready = Output(Bool())
}

class AUBundle(implicit p: AUParams) extends CmdBundle {
  val cmd = Input(UInt(CmdList.CmdLen.W))
}

class ArithmeticUnit(implicit p: AUParams) extends Module {
  val cycleCounter = Module(new CycleCounter)
  val io = IO(new AUBundle)
  val cmd = RegInit(0.U(CmdList.CmdLen.W))
  val submodules = Seq(Module(new Addition), Module(new Subtraction), Module(new Multiplication), Module(new Division))
  val sub = VecInit(submodules.map(_.io))

  sub.foreach(s => {
    s.valid := false.B
    s.op1 := 0.S
    s.op2 := 0.S
  })

  cmd := io.cmd
  io.ready := !io.valid

  when(io.valid) {
    sub(io.cmd).valid := true.B
    sub(io.cmd).op1 := io.op1
    sub(io.cmd).op2 := io.op2
  }

  io.out := Mux(sub(cmd).ready, sub(cmd).out, 0.S)
}

abstract class AUSubmodule(implicit p: AUParams) extends Module {
  val io = IO(new CmdBundle)
  val res = RegInit(0.S(p.width.W))

  def Operation(op1: SInt, op2: SInt): SInt
  def Trace = {
    printf(
      cf" Module:" + this
        .getClass()
        .getName() + cf"\n  valid: ${io.valid}%d, op1: ${io.op1}%d, op2: ${io.op2}%d, out: ${io.out}%d, ready: ${io.ready}%d\n"
    )
  }

  io.ready := !io.valid

  when(io.valid) {
    res := Operation(io.op1, io.op2)
  }

  io.out := res

  if (p.trace) Trace
}

class Addition(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 + op2
  }
}

class Subtraction(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 - op2
  }
}

class Multiplication(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 * op2
  }
}

class Division(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 / op2
  }
}

class CycleCounter(implicit p: AUParams) extends Module {
  val counter = RegInit(0.U(32.W))
  counter := counter + 1.U
  if (p.trace) printf(cf"[Cycle: $counter%d]\n")
}
