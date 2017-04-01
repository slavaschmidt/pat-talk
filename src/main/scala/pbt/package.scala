import scala.util.Random

package object pbt {
  def random[A](xs: Iterable[A]): A = {
    val xxs = xs.toIndexedSeq
    xxs(Random.nextInt(xxs.size))
  }

  trait Point {
    def x: Int
    def y: Int
  }

  trait Rectangle {
    def w: Int
    def h: Int
    def contains(p: Point): Boolean = p.x >= 0 && p.y >= 0 && p.x < w && p.y < h
    def idx(p: Point): Int = p.y * w + p.x
  }

}
