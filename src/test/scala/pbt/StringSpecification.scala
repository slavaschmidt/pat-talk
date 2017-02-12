package pbt

import org.junit.Assert
import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

import scala.util.Try

object StringSpecification extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length > a.length && (a+b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

  property("jUnit double asserts") = forAll { (expected: Double, actual: Double, delta: Double) =>
    val a = Try { Assert.assertEquals(expected, actual, delta) }
    val b = Try { Assert.assertEquals(actual, expected, delta) }
    a == b
  }

  property("jUnit float asserts") = forAll { (expected: Float, actual: Float, delta: Float) =>
    val a = Try { Assert.assertEquals(expected, actual, delta) }
    val b = Try { Assert.assertEquals(actual, expected, delta) }
    a == b
  }
}
