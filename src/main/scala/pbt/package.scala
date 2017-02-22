import scala.util.Random

package object pbt {
  def random[A](xs: Iterable[A]): A = {
    val xxs = xs.toIndexedSeq
    xxs(Random.nextInt(xxs.size))
  }

}
