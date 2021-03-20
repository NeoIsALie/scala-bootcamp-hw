package basics

import basics.ControlStructuresHomework._
import org.scalatest.flatspec.AnyFlatSpec

class ControlStructuresTest extends AnyFlatSpec {


  "divide" should "be ok" in {
    assert(process("divide 4 5") == "4 divided by 5 is 0.8")
  }

  "divide on zero" should "be ok" in {
    assert(process("divide 4 0") == "Division by zero")
  }

  "divide incorrect number of arguments" should "be ok" in {
    assert(process("divide 4 5 3") == "Division requires 2 operands")
  }

  "sum" should "be ok" in {
    assert(process("sum 5 5 6 8.5") == "the sum of 5 5 6 8.5 is 24.5")
  }


  "average" should "be ok" in {
    assert(process("average 4 3 8.5 4") == "the average of 4 3 8.5 4 is 4.875")
  }

  "no arguments passed" should "be ok" in {
    assert(process("average") == "Not enough operands")
  }

  "min" should "be ok" in {
    assert(process("min 4 -3 -17") == "the minimum of 4 -3 -17 is -17")
  }

  "max" should "be ok" in {
    assert(process("max 4 -3 -17") == "the maximum of 4 -3 -17 is 4")
  }

  "wrong arguments" should "be ok" in {
    assert(process("sum asd 2 3") == "Impossible to convert to numbers")
  }

}
