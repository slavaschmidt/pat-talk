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
    def width: Int
    def height: Int
    def contains(p: Point): Boolean = p.x >= 0 && p.y >= 0 && p.x < width && p.y < height
    def idx(p: Point): Int = p.y * width + p.x


  }

}
