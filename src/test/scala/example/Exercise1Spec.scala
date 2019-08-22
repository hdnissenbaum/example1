package example

import scala.util.Random

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Exercise1Spec extends FlatSpec with Matchers {

  "Exercise1" should "find first in a list of 1" in {
    val rand = Random
    val number = rand.between(-1000, 1000)
    Exercise1.locateFirst(List(number)) shouldEqual ((number, 0))
  }

  "Exercise1" should "find first in a list of same number" in {
    val rand = Random
    val size = rand.between(100, 200)
    val number = rand.between(-1000, 1000)
    val integers = Range(0, size).map( _ => number ).sorted.toList
    Exercise1.locateFirst(integers) shouldEqual ((number, 0))
  }

  "Exercise1" should "find first in an unshifted ordered list" in {
    val rand = Random
    val size = rand.between(100, 200)
    val integers = Range(0, size).map( _ => rand.between(-1000, 1000) ).sorted.toList
    Exercise1.locateFirst(integers) shouldEqual ((integers.head, 0))
  }

  "Exercise1" should "find first in a shifted ordered list" in {
    val rand = Random
    val size = rand.between(100, 200)
    val integers = Range(0, size).map( _ => rand.between(-1000, 1000) ).sorted.toList
    val rotateAt = rand.between(0, size)
    val (a, b) = integers.splitAt( rotateAt )
    val input = b ::: a
    Exercise1.locateFirst(input) shouldEqual ((a.head, b.length))
  }


}
