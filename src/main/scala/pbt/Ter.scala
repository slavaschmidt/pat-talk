package pbt

import java.io.{PrintStream, Writer}

import Ansi._

class Term(out: PrintStream) {

  def csi(n: Int, c: Char) = out.print(Ansi.csi + n + c)

  def up(n: Int) = if (n > 0) csi(n, 'A')

  def down(n: Int) = if (n > 0) csi(n, 'B')

  def right(n: Int) = if (n > 0) csi(n, 'C')

  def left(n: Int) = if (n > 0) csi(n, 'D')

  def startUp(n: Int) = if (n > 0) csi(n, 'F')

  def col(n: Int) = if (n > 0) csi(n, 'G')

  def pos(n: Int, m: Int) = if (n > 0 && m > 0) out.print(s"${Ansi.csi}$n;${m}H")

  def savePos = out.print(s"${Ansi.csi}s")

  def restorePos = out.print(s"${Ansi.csi}u")

  /*
    * n=0: clear from cursor to end of screen
    * n=1: clear from cursor to the beginning of screen
    * n=2: clear entire screen
    * n=3: as 2 but clear scrollback as well
    */
  def eraseDisplay(n: Int) = csi(n, 'J')

  /*
    * Clear the current line without changing cursor position
    *
    * n=0: clear from cursor to end of line
    * n=1: clear from cursor to start of line
    * n=2: clear entire line
    */
  def eraseLine(n: Int) = csi(n, 'K')




  def backgroundColor(color: Int) = s"${Ansi.csi}48;5;${color}m"

  def formatText(str: String)(txtColor: Int, backColor: Int) =
    s"${textColor(txtColor)}${backgroundColor(backColor)}${str}${resetColor}"

  def formatTxt(str: String)(txtColor: Int) = s"${textColor(txtColor)}${str}${resetColor}"



}

object Ansi {
  val csi = "\033["
  def textColor(color: Int) = s"${csi}38;5;${color}m"
  val resetColor = s"${csi}0m"
  val resetUnderline = "\u001b[24m"
  val resetForegroundColor = "\u001b[39m"
  val resetBackgroundColor = "\u001b[49m"

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

  val orange = 166
}
