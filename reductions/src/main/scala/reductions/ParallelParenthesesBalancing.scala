package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  private def getValue(char: Char): Int = char match {
    case '(' => 1
    case ')' => -1
    case _   => 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {

    @tailrec
    def balanceAcc(chars: Array[Char], acc: Int): Boolean =
      if (acc < 0) false
      else if (chars.isEmpty) if (acc == 0) true else false
      else balanceAcc(chars.tail, acc + getValue(chars.head))

    balanceAcc(chars, 0)

  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) (arg1, arg2)
      else {
        val delta = arg1 + getValue(chars(idx))
        val depth = arg2.min(delta)
        traverse(idx + 1, until, delta, depth)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + ((until - from) / 2)
        val ((delta1, depth1), (delta2, depth2)) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        (delta1 + delta2, depth1.min(delta1 + depth2))
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

}
