package ArithmeticUnit

import chisel3._
import chisel3.util._

//перечисление доступных команд
object CmdList {
  def Addition = 0.U
  def Subtraction = 1.U
  def Multiplication = 2.U
  def Division = 3.U

  def CmdCount = 4
  def CmdLen = log2Ceil(CmdList.CmdCount)
}

//параметры арифметического устройства
//при компиляции они интерпретируются как константы
case class AUParams(
    width: Int = 32,
    trace: Boolean = false
)

//интерфейсы входа выхода
class CmdBundle(implicit p: AUParams) extends Bundle {
  val valid = Input(Bool()) // сигнал валидности
  val op1 = Input(SInt(p.width.W)) // операнд 1
  val op2 = Input(SInt(p.width.W)) // операнд 2
  val out = Output(SInt(p.width.W)) // результат
  val ready = Output(Bool()) // сигнал готовности
  val overflow = Output(Bool()) // сигнал переполнения
}

class AUBundle(implicit p: AUParams) extends CmdBundle {
  val cmd = Input(UInt(CmdList.CmdLen.W)) // код команды
}

//арифметическое устройство
class ArithmeticUnit(implicit p: AUParams) extends Module {
  val cycleCounter = Module(new CycleCounter) // счетчик тактов для отладки
  val io = IO(new AUBundle) // IO интерфейсы
  val cmd = RegInit(0.U(CmdList.CmdLen.W)) // регистр для хранения кода команды
  val submodules = Seq(
    Module(new Addition),
    Module(new Subtraction),
    Module(new Multiplication),
    Module(new Division)
  ) // экземпляры субмодулей

  // для демультиплексирования создаем вектор IO интерфейсов
  val sub = VecInit(submodules.map(_.io))

  // зануляем входы субмодулей
  sub.foreach(s => {
    s.valid := false.B
    s.op1 := 0.S
    s.op2 := 0.S
  })

  cmd := io.cmd // записываем в регистр код команды
  io.ready := !io.valid

  // если есть разрешающий сигнал, то передаем данные на субмодуль
  // в соответствии с кодом команды
  when(io.valid) {
    sub(io.cmd).valid := true.B
    sub(io.cmd).op1 := io.op1
    sub(io.cmd).op2 := io.op2
  }

  // выводим результат и переполнение, если субмодуль готов
  io.out := Mux(sub(cmd).ready, sub(cmd).out, 0.S)
  io.overflow := Mux(sub(cmd).ready, sub(cmd).overflow, false.B)
}

//субмодуль арифметического устройства
abstract class AUSubmodule(implicit p: AUParams) extends Module {
  val io = IO(new CmdBundle) // IO интерфейсы
  val res = RegInit(0.S(p.width.W)) // регистр для результата
  val overflow = RegInit(false.B) // регистр для переполнения

  // виртуальные функции для доопределения в наследнике
  def Operation(op1: SInt, op2: SInt): SInt
  def Overflow(op1: SInt, op2: SInt): Bool

  // функции для проверки на переполнения
  def MaxValue(width: Int) = {
    (1.S << (width - 1)) - 1.S
  }
  def MinValue(width: Int) = {
    -(1.S << (width - 1))
  }

  // функция для печати отладочной информации
  def Trace = {
    printf(
      cf" Module:" + this
        .getClass()
        .getName() + cf"\n  valid: ${io.valid}%d, op1: ${io.op1}%d, op2: ${io.op2}%d, out: ${io.out}%d, ready: ${io.ready}%d, overflow: ${io.overflow}, overflowedRes: ${Operation(io.op1, io.op2)}%d\n"
    )
  }

  io.ready := !io.valid

  // если передан разрешающий сигнал, то записать результат и переполнение
  when(io.valid) {
    val overflowWire = Overflow(io.op1, io.op2)
    val tempres = Operation(io.op1, io.op2)
    res := tempres(p.width - 1, 0).asSInt
    overflow := overflowWire
  }

  // передали результат и переполнение на выход
  io.out := res
  io.overflow := overflow
  if (p.trace) Trace
}

//Сумматор с доопределенными функциями
class Addition(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 +& op2 // сумма с расширением на 1 бит
  }

  // если результат операции вышел за допустимые пределы, то переполнение
  override def Overflow(op1: SInt, op2: SInt): Bool = {
    val res = Operation(op1, op2)
    val right = res > MaxValue(p.width - 1)
    val left = res < MinValue(p.width - 1)
    left || right
  }
}

//Вычитатель с доопределенными функциями
class Subtraction(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 -% op2 // вычитание с расширением на 1 бит
  }
  override def Overflow(op1: SInt, op2: SInt): Bool = {
    val res = Operation(op1, op2)
    val right = res > MaxValue(p.width - 1)
    val left = res < MinValue(p.width - 1)
    left || right
  }
}

//умножитель с доопределенными функциями
class Multiplication(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 * op2
  }
  override def Overflow(op1: SInt, op2: SInt): Bool = {
    val res = Operation(op1, op2)
    val right = res > MaxValue(p.width - 1)
    val left = res < MinValue(p.width - 1)
    left || right
  }
}

//Делитель с доопределенными функциями
class Division(implicit p: AUParams) extends AUSubmodule {
  override def Operation(op1: SInt, op2: SInt): SInt = {
    op1 / op2
  }
  override def Overflow(op1: SInt, op2: SInt): Bool = {
    val zero = op2 === 0.S // проверка на деление на 0
    val res = Operation(op1, op2)
    val right = res > MaxValue(p.width - 1)
    val left = res < MinValue(p.width - 1)
    left || right || zero
  }
}

class CycleCounter(implicit p: AUParams) extends Module {
  val counter = RegInit(0.U(32.W))
  counter := counter + 1.U
  if (p.trace) printf(cf"[Cycle: $counter%d]\n")
}
