package basics

import scala.io.Source
import basics.ControlStructuresHomework.Command._
import basics.ControlStructuresHomework.Result._

object ControlStructuresHomework {


  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  final case class ErrorMessage(value: String)

  sealed trait Result

  object Result {

    final case class DivideResult(dividend: Double, divisor: Double, result: Double) extends Result

    final case class SumResult(numbers: List[Double], result: Double) extends Result

    final case class AverageResult(numbers: List[Double], result: Double) extends Result

    final case class MinResult(numbers: List[Double], result: Double) extends Result

    final case class MaxResult(numbers: List[Double], result: Double) extends Result

  }

  def parseCommand(input: String): Either[ErrorMessage, Command] = {
    input.replaceAll("\\s{2,}", " ").trim.split(" ").toList match {
      case list if list.length < 2 => Left(ErrorMessage("Error: not enough operands for command"))
      case _ :: xs if xs.map(_.toDoubleOption).contains(None) => Left(ErrorMessage("Error: impossible to convert to numbers"))
      case x :: xs if x == "divide" => xs match {
        case twoOperands if twoOperands.length != 2 => Left(ErrorMessage("Error: division requires only two operands"))
        case _ => Right(Divide(xs.head.toDouble, xs.last.toDouble))
      }
      case x :: xs if x == "sum" => Right(Sum(xs.map(elem => elem.toDouble)))
      case x :: xs if x == "average" => Right(Average(xs.map(elem => elem.toDouble)))
      case x :: xs if x == "min" => Right(Min(xs.map(elem => elem.toDouble)))
      case x :: xs if x == "max" => Right(Max(xs.map(elem => elem.toDouble)))
      case _ => Left(ErrorMessage("Error: unexpected error"))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = {
    command match {
      case Divide(dividend, divisor) => (dividend, divisor) match {
        case (_, 0) => Left(ErrorMessage("Error: division by zero"))
        case _ => Right(DivideResult(dividend, divisor, dividend / divisor))
      }
      case Sum(numbers) =>
        Right(SumResult(numbers, numbers.sum))
      case Average(numbers) =>
        Right(AverageResult(numbers, numbers.sum / numbers.size))
      case Min(numbers) =>
        Right(MinResult(numbers, numbers.min))
      case Max(numbers) =>
        Right(MaxResult(numbers, numbers.max))
      case _ => Left(ErrorMessage("Unexpected error"))
    }
  }

  def renderResult(x: Result): String = {
    val formatter : Double => String = (number: Double) => {
      if (number == number.asInstanceOf[Long]) number.formatted("%.0f") else number.toString
    }

    x match {
      case DivideResult(dividend, divisor, result) =>
        s"${formatter(dividend)} divided by ${formatter(divisor)} is ${formatter(result)}"
      case SumResult(numbers, result) =>
        s"the sum of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
      case AverageResult(numbers, result) =>
        s"the average of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
      case MinResult(numbers, result) =>
        s"the minimum of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
      case MaxResult(numbers, result) =>
        s"the maximum of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
    }
  }

  def process(x: String): String = {
    val result = for {
      parsedCommand <- parseCommand(x)
      calculatedCommand <- calculate(parsedCommand)
    } yield calculatedCommand

    result match {
      case Left(error) => s"Error: ${error.value}"
      case Right(result) => renderResult(result)
    }

  }
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
