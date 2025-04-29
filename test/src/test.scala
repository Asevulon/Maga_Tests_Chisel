package ArithmeticUnit.test

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ArithmeticUnit._

//класс с тестами
class Test extends AnyFlatSpec with ChiselScalatestTester {
  // объявление теста
  "Arithmetic Unit" should "do 1 + 2" in {
    // параметры устройства
    implicit val p = new AUParams()
    // начало теста
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true) // подали разрешающий сигнал
      au.io.cmd.poke(CmdList.Addition) // подали код команды
      au.io.op1.poke(1) // подали операнд
      au.io.op2.poke(2) // подали операнд
      au.clock.step() // шаг по времени

      au.io.valid.poke(false) // убрали разрешающий сигнал
      au.io.out.expect(3) // проверили результат
      au.io.ready.expect(true) // проверили готовность
      au.io.overflow.expect(false) // проверили переполнение

      au.clock.step()
    }
  }

  it should "do 2 * 3" in {
    implicit val p = new AUParams()
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Multiplication)
      au.io.op1.poke(2)
      au.io.op2.poke(3)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.out.expect(6)
      au.io.ready.expect(true)
      au.io.overflow.expect(false)

      au.clock.step()
    }
  }

  it should "do 5 - 6" in {
    implicit val p = new AUParams()
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Subtraction)
      au.io.op1.poke(5)
      au.io.op2.poke(6)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.out.expect(-1)
      au.io.ready.expect(true)
      au.io.overflow.expect(false)

      au.clock.step()
    }
  }

  it should "do 10 / 2" in {
    implicit val p = new AUParams()
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Division)
      au.io.op1.poke(10)
      au.io.op2.poke(2)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.out.expect(5)
      au.io.ready.expect(true)
      au.io.overflow.expect(false)

      au.clock.step()
    }
  }
}

class TestOverflow extends AnyFlatSpec with ChiselScalatestTester {

  "Arithmetic Unit" should "Overflow on 7+1" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Addition)
      au.io.op1.poke(7)
      au.io.op2.poke(1)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }

  it should "Overflow on -8  + -1" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Addition)
      au.io.op1.poke(-8)
      au.io.op2.poke(-1)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }

  it should "Overflow on -8 -1" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Subtraction)
      au.io.op1.poke(-8)
      au.io.op2.poke(1)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }

  it should "Overflow on 7 - -1" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Subtraction)
      au.io.op1.poke(7)
      au.io.op2.poke(-1)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }

  it should "Overflow on 2 * 4" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Multiplication)
      au.io.op1.poke(2)
      au.io.op2.poke(4)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }

  it should "Overflow on -3 3" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Multiplication)
      au.io.op1.poke(-3)
      au.io.op2.poke(3)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }

  it should "Overflow on 1 / 0" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Division)
      au.io.op1.poke(1)
      au.io.op2.poke(0)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }

  it should "Overflow on -8 / -1" in {
    implicit val p = AUParams(width = 4, trace = true)
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Division)
      au.io.op1.poke(-8)
      au.io.op2.poke(-1)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.ready.expect(true)
      au.io.overflow.expect(true)

      au.clock.step()
    }
  }
}
