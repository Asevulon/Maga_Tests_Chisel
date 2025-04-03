package ArithmeticUnit.test

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ArithmeticUnit._

class Test extends AnyFlatSpec with ChiselScalatestTester {
  "Arithmetic Unit" should "do 1 + 2" in {
    implicit val p = new AUParams()
    test(new ArithmeticUnit()) { au =>
      au.io.valid.poke(true)
      au.io.cmd.poke(CmdList.Addition)
      au.io.op1.poke(1)
      au.io.op2.poke(2)
      au.clock.step()

      au.io.valid.poke(false)
      au.io.out.expect(3)
      au.io.ready.expect(true)

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

      au.clock.step()
    }
  }
}
