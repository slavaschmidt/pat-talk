import org.junit.Assert
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import scala.util.Try

object BasicProperties extends Properties("String") {

  property("testIntAplusB") = forAll { (a: Int, b: Int) =>
      a + b == b + a
  }

  property("testFloatAplusB") = forAll { (a: Float, b: Float) =>
      a + b == b + a
  }

  property("jUnit float asserts") = forAll { (a: Float, b: Float, delta: Float) =>
    val left = Try { Assert.assertEquals(a, b, delta) }
    val right = Try { Assert.assertEquals(b, a, delta) }
    left == right
  }
  

}
