package pbt

import scala.collection.immutable.Range.Inclusive

object Ansi {
  val csi = "\033["
  val resetColor = s"${csi}0m"
  val resetForegroundColor = "\u001b[39m"
  def textColor(color: Int) = s"${csi}38;5;${color}m"

  val black = 0
  val maroon = 1
  val green = 2
  val brown = 3
  val navy = 4
  val magenta = 5
  val teal = 6
  val silver = 7
  val gray = 8
  val red = 9
  val lime = 10
  val yellow = 11
  val blue = 12
  val pink = 13
  val cyan = 14
  val white = 15

  val colors: Inclusive = 1 to 15
}
