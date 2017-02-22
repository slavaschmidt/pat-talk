import org.scalatest._
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable.Stack

// http://www.scalatest.org/user_guide/property_based_testing

class Fraction(n: Int, d: Int) {

  require(d != 0)
  require(d != Integer.MIN_VALUE)
  require(n != Integer.MIN_VALUE)

  val numer = if (d < 0) -1 * n else n
  val denom = d.abs

  override def toString = numer + " / " + denom
}

class ScalaTest extends FlatSpec with Matchers with PropertyChecks {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be(2)
    stack.pop() should be(1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a[NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }

  forAll { (n: Int, d: Int) =>

    whenever(d != 0 && d != Integer.MIN_VALUE
      && n != Integer.MIN_VALUE) {

      val f = new Fraction(n, d)

      if (n < 0 && d < 0 || n > 0 && d > 0)
        f.numer should be > 0
      else if (n != 0)
        f.numer should be < 0
      else
        f.numer === 0

      f.denom should be > 0
    }
  }

  val invalidCombos = Table(
    ("n", "d"),
    (Integer.MIN_VALUE, Integer.MIN_VALUE),
    (1, Integer.MIN_VALUE),
    (Integer.MIN_VALUE, 1),
    (Integer.MIN_VALUE, 0),
    (1, 0)
  )

  forAll(invalidCombos) { (n: Int, d: Int) =>
    an[IllegalArgumentException] should be thrownBy {
      new Fraction(n, d)
    }
  }
}
