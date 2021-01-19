object PartialSumBitOp extends App {
  val a = Seq(1, 10, 49, 3, 8, 13, 7, 23, 60, -500, 42, 599, 45, -23, 1, 10, 49, 3, 8, 13)
  val n = a.length
  val k = 444

  var isMatch = false
  var bitsCounter = 0
  val max = ~(-1 << n)
  while (!isMatch && bitsCounter <= max) {
    var sum = 0
    for (i <- 0 to (n - 1)) {
      val mask = 1 << i
      val masked = bitsCounter & mask
      if (masked != 0) sum = sum + a(i)
    }
    if (sum == k) {
      isMatch = true
    } else {
      bitsCounter = bitsCounter + 1
    }
  }

  if (isMatch) {
    var result: Seq[Int] = Seq()
    for (i <- 0 to (n - 1)) {
      val mask = 1 << i
      val masked = bitsCounter & mask
      if (masked != 0) result = result :+ a(i)
    }
    println(s"Yes ${result}")
  } else {
    println("No")
  }
}
