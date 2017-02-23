package pbt

import pbt.Demo._
import java.io.PrintStream
import pbt.Ansi.{resetColor, textColor}
import scala.language.implicitConversions

object SpeakerCheat {
  val term = new Term(System.out)

  def printBoard(board: Board) = {
    term.eraseDisplay(2)
    term.pos(1, 1)
    println(drawBoard(board))
  }

  def drawBoard(board: Board) = {
    type Border = (String, String, String)
    val topLine = ("┌", "┬", "┐\n")
    val midLine = ("├", "┼", "┤\n")
    val botLine = ("└", "┴", "┘\n")


    def drawLine(border: Border, drawLine: Boolean = true)(row: Seq[RichCell]) = {
      val line: String = row.init.map { _.topChar + border._2 }.mkString
      val topLine = border._1 + line + row.last.topChar + border._3
      val bottomLine =
        if (drawLine)
          row.map { c => c.leftChar + c.center }.mkString + row.last.mirror.leftChar
        else ""
      topLine + bottomLine + "\n"
    }

    class RichBoard(b: Board) extends Board(b.width, b.height, b.cells) {
      def row(i: Int) = cells.slice(i * width, i * width + width)
    }

    implicit def richBoard(b: Board): RichBoard = new RichBoard(b)

    def fullBoard = {
      drawLine(topLine)(board.row(0).map(richCell)) +
        (for { i <- 1 until board.height } yield drawLine(midLine)(board.row(i).map(richCell))).mkString +
        drawLine(botLine, drawLine = false)(board.row(board.height-1).map(richCell).map(_.mirror))
    }

    fullBoard
  }


  implicit def richCell(cell: Cell): RichCell = RichCell(cell)

  case class RichCell(cell: Cell, up: Border = Top, left: Border = Left) {
    def topChar = if (cell.borders.contains(up)) "─" else " "
    def leftChar = if (cell.borders.contains(left)) "│" else " "
    def center = cell.owner.map(o => color(o.color) + "█" + resetColor).getOrElse(" ")
    def color(c: Int) = Ansi.textColor(c)
    def resetColor = Ansi.resetColor
    def mirror: RichCell = RichCell(cell, Bottom, Right)
  }
}

object RunnableDemo extends App {

  val players = Seq(Player(Ansi.green, randomStrategy), Player(Ansi.yellow, randomStrategy), Player(Ansi.navy, finishingStrategy), Player(Ansi.red, fullStrategy))
  val board = new Board(15, 15)
  val (winners, log) = game(players, board)

  val term = new Term(System.out)

  log.foreach { l =>
    SpeakerCheat.printBoard(l)
    Thread.sleep(20)
  }


}


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
