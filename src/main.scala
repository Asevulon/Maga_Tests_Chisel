package ArithmeticUnit

import chisel3._

object Main extends App {

  implicit val p: AUParams = AUParams()
  emitVerilog(new ArithmeticUnit, Array[String]("--target-dir", "Verbuild"))

}
