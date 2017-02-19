package pbt

import pbt.demo._
import scratch.Term

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

  case class RichCell(cell: Cell, up: Border = Top, left: Border = demo.Left) {
    def topChar = if (cell.borders.contains(up)) "─" else " "
    def leftChar = if (cell.borders.contains(left)) "│" else " "
    def center = cell.owner.map(o => color(o.color) + "█" + resetColor).getOrElse(" ")
    def color(c: Int) = Ansi.textColor(c)
    def resetColor = Ansi.resetColor
    def mirror: RichCell = RichCell(cell, Bottom, demo.Right)
  }
}

object RunnableDemo extends App {

  val players = Seq(demo.Player(Ansi.green, demo.fullStrategy), demo.Player(Ansi.yellow, demo.fullStrategy), demo.Player(Ansi.navy, demo.fullStrategy), demo.Player(Ansi.red, demo.fullStrategy))
  val board = Board(15, 15, _ => demo.EmptyCell(Set.empty))
  val (winners, log) = demo.game(players, board)

  val term = new Term(System.out)
  term.savePos

  log.foreach { l =>
    SpeakerCheat.printBoard(l)
    Thread.sleep(10)
  }
  term.eraseDisplay(2)
  term.restorePos
  println(log.size)
  println(winners)
}
