package pbt

import java.io.File

import org.specs2.{ScalaCheck, Specification}

import org.scalacheck._
import org.scalacheck.util.Pretty

import scalaz.syntax.applicative._

class HelloWorldSpec extends Specification {
  def is =
    s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   contain 11 characters                                         $e1
   start with 'Hello'                                            $e2
   end with 'world'                                              $e3
                                                                 """

  def e1 = "Hello world" must have size (11)

  def e2 = "Hello world" must startWith("Hello")

  def e3 = "Hello world" must endWith("world")
}

// https://etorreborre.github.io/specs2/guide/SPECS2-3.8.8/org.specs2.guide.UseScalaCheck.html
class PropertyBasedSpec extends Specification with ScalaCheck {
  def is =
    s2"""
    addition and multiplication are related $p1
    addition and multiplication are related $p2
    """

  val p1: Prop = Prop.forAll { (a: Int) => a + a == 2 * a }

  val p2: Properties = new Properties("addition/multiplication") {
    property("addition1") = Prop.forAll { (a: Int) => a + a == 2 * a }
    property("addition2") = Prop.forAll { (a: Int) => a + a + a == 3 * a }
  }

  {
    // a Boolean
    s2"addition and multiplication are related ${prop { (a: Int) => a + a == 2 * a }}"

    // a MatchResult
    s2"addition and multiplication are related ${prop { (a: Int) => a + a must_== 2 * a }}"

    // a Prop
    s2"addition and multiplication are related ${prop { (a: Int) => (a > 0) ==> (a + a must_== 2 * a) }}"
  }

  {
    // ScalaCheck requires an implicit Arbitrary[T] instance for each parameter of type T used in a property.
    // If you rather want to pick up a specific Arbitrary[T] for a given property
    // argument you can modify the prop with to use another Arbitrary instance:

    s2"""
    a simple property       $ex1
    a more complex property $ex2
  """

    def abStringGen = (Gen.oneOf("a", "b") |@| Gen.oneOf("a", "b")) (_ + _)

    implicit def abStrings: Arbitrary[String] = Arbitrary(abStringGen)

    def ex1 = prop((s: String) => s must contain("a") or contain("b")).setArbitrary(abStrings)

    // use the setArbitrary<n> method for the nth argument
    def ex2 = prop((s1: String, s2: String) => (s1 + s2) must contain("a") or contain("b")).
      setArbitrary1(abStrings).setArbitrary2(abStrings)
  }

  {
    // It is also possible to pass a Gen[T] instance instead of an Arbitrary[T]:

    val abStringGen = (Gen.oneOf("a", "b") |@| Gen.oneOf("a", "b")) (_ + _)

    def ex1 = prop((s: String) => s must contain("a") or contain("b")).setGen(abStringGen)

  }

  {
    // Specific Shrink and Pretty instances can also be specified at the property level:
    val myShrinkString: Shrink[String] = Shrink.shrinkString

    // set a specific shrink instance on the second parameter
    prop((s1: String, s2: String) => s1.nonEmpty or s2.nonEmpty).setShrink2(myShrinkString)

    // set a specific pretty instance
    prop((s: String) => s must contain("a") or contain("b")).setPretty((s: String) =>
      Pretty((prms: Pretty.Params) => if (prms.verbosity >= 1) s.toUpperCase else s))

    // or simply if you don't use the Pretty parameters
    prop((s: String) => s must contain("a") or contain("b")).pretty((_: String).toUpperCase)

  }

  implicit def file: Arbitrary[File] = Arbitrary(Gen.const(File.createTempFile("bla", "bla"))) // by-name parameter

  {
    // ScalaCheck properties are sometimes used to test stateful applications rather than pure functions.
    // For example you want to test that a function is writing files somewhere and you would
    // like those files to be deleted after each property execution:

    def createFile(f: File): Unit = { /* create file here */ }

    def deleteTmpDir(): Unit = { /* delete temp folder here */ }

    prop { f: File =>
      createFile(f)
      f.exists
    }.after(deleteTmpDir()) // before and beforeAfter can also be used there

  }

  {
    // You can also “prepare” the property to be tested based on the generated arguments:

    def createFile(directory: File, f: File): Unit =  { /* create file here */ }

    // this method will keep the arguments intact but can
    // have a side-effect to prepare the system
    def setupDirectoryAndFile = (directory: File, file: File) => (directory, file)

    prop { (directory: File, f: File) =>
      createFile(directory, f)
      f.exists
    }.prepare(setupDirectoryAndFile)

  }
}
