package pbt

import pbt.Demo.{Board, Border, Cell}

object SpeakerCheat {
  val term = new Term(System.out)

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

    def fullBoard = {
      drawLine(topLine)(board.row(0).map(richCell)) +
        (for { i <- 1 until board.height } yield drawLine(midLine)(board.row(i).map(richCell))).mkString +
        drawLine(botLine, drawLine = false)(board.row(board.height-1).map(richCell).map(_.mirror))
    }

    def printBoard = {
      term.eraseDisplay(2)
      term.pos(1, 1)
      println(fullBoard)
    }

    fullBoard
  }

  implicit def richCell(cell: Cell): RichCell = RichCell(cell)

  case class RichCell(cell: Cell, up: Border = Demo.Up, left: Border = Demo.Left) {
    def topChar = if (cell.borders.contains(up)) "─" else " "
    def leftChar = if (cell.borders.contains(left)) "│" else " "
    def center = cell.user.map(o => color(o.color) + "█" + resetColor).getOrElse(" ")
    def color(c: Int) = Ansi.textColor(c)
    def resetColor = Ansi.resetColor
    def mirror: RichCell = RichCell(cell, Demo.Down, Demo.Right)
  }
}
