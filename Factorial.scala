import scala.math.BigInt

object Factorial extends App {
  def factorial(i: BigInt): BigInt = if (i == 0) 1 else i * factorial(i - 1)

  println(factorial(1000))
}
