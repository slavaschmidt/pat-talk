package pbt

import java.awt.event.ActionEvent

import jline.TerminalFactory
import jline.console.{ConsoleReader, KeyMap}

import scala.collection.mutable

case class User(color: Int)

trait Cell {
  def top: Boolean
  def bottom: Boolean
  def left: Boolean
  def right: Boolean
  def owner: Option[User]

  def fullBorder = top && bottom && left && right
  def noBorder = !top && !bottom && !left && !right

  def mirror: Cell

  def topChar = if (top) "─" else " "
  def leftChar = if (left) "│" else " "
  def center = owner.map(o => color(o.color) + "█" + resetColor).getOrElse(" ")

  val t = new Term(System.out)
  def color(c: Int) = Ansi.textColor(c)
  def resetColor = Ansi.resetColor
}

object EmptyCell {
  def apply = new EmptyCell(false, false, false, false)
}
case class EmptyCell(top: Boolean, bottom: Boolean, left: Boolean, right: Boolean) extends Cell {
  override val owner = None
  def mirror = EmptyCell(bottom, top, right, left)
}
case class FullCell(user: User) extends Cell {
  val owner = Some(user)
  val top = true
  val bottom = true
  val left = true
  val right = true
  val mirror = this
}

class Board(width: Int, height: Int) {

  val state = mutable.ArrayBuffer.fill[Cell](height, width)(EmptyCell.apply)

  type Border = (String, String, String)
  val topLine = ("┌", "┬", "┐\n")
  val midLine = ("├", "┼", "┤\n")
  val botLine = ("└", "┴", "┘\n")


  def drawLine(border: Border, drawLine: Boolean = true)(row: Seq[Cell]) = {
    val line: String = row.init.map { _.topChar + border._2 }.mkString
    val topLine = border._1 + line + row.last.topChar + border._3
    val bottomLine =
      if (drawLine)
        row.map { c => c.leftChar + c.center }.mkString + row.last.mirror.leftChar
      else ""
    topLine + bottomLine + "\n"
  }

  def drawBoard = {
    drawLine(topLine)(state.head) +
      state.tail.map(drawLine(midLine)).mkString +
      drawLine(botLine, drawLine = false)(state.last.map(_.mirror))
  }

  def printBoard = println(drawBoard)
}

object Test extends App {


  sealed trait Action {
    def key: String
  }

  object Action {
    case class Up(key: String = "\033[0A") extends Action
    case class Left(key: String = "\033[0B") extends Action
    case class Right(key: String = "\033[0C") extends Action
    case class Down(key: String = "\033[0D") extends Action
  }

  val pathedStty = if (new java.io.File("/bin/stty").exists()) "/bin/stty" else "stty"

  private def sttyCmd(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"$pathedStty $s < /dev/tty"): ProcessBuilder
  }

  def stty(s: String) = sttyCmd(s).!!


  def init() = {
    stty("-a")

    //    Debug("Initializing, Width " + width)
    //    Debug("Initializing, Height " + height)
    val initialConfig = stty("-g").trim
    // stty("-icanon min 1 -icrnl -inlcr -ixon")
    // stty("-echo")
    // stty("intr undef")
    // (width, height, initialConfig)
  }

  // init()

  val terminal = TerminalFactory.get()
  terminal.init()

  val reader = new ConsoleReader()

  val map = new KeyMap("game_keys")
  val term = new Term(System.out)

  var x = 1
  var y = 1

  map.bind(IndexedSeq(27, 91, 65).map(_.toChar), () => { term.up(1); y -= 1 })
  map.bind(IndexedSeq(27, 91, 68).map(_.toChar), () => { term.left(1); x-=1 } )
  map.bind(IndexedSeq(27, 91, 67).map(_.toChar), () => { term.right(1); x+=1 } )
  map.bind(IndexedSeq(27, 91, 66).map(_.toChar), () => { term.down(1); y+=1 } )
  map.bind(IndexedSeq(13).map(_.toChar), () => enterPressed)
  map.bind("q", () => sys.exit())

  term.eraseDisplay(2)
  term.pos(1, 1)
  val board = new Board(20, 20)
  board.printBoard
  term.pos(1, 1)

  def enterPressed = {
    val xc = (x-1)/2
    val yc = (y-1)/2
    val xBorder = x % 2
    val yBorder = y % 2
    val cell = board.state(yc)(xc)
    term.savePos
    term.right(80)
    (cell, xBorder, yBorder) match {
      case (c @ EmptyCell(_, _, false, _), 1, _) =>
        board.state(yc)(xc) = c.copy(left=true)
        if (xc>0) board.state(yc)(xc-1) match {
          case cc: EmptyCell => board.state(yc)(xc-1) = cc.copy(right=true)
          case _ =>
        }
      case (c @ EmptyCell(false, _, _, _), _, 1) =>
        board.state(yc)(xc) = c.copy(top=true)
        if (yc >0)  board.state(yc-1)(xc) match {
          case cc: EmptyCell => board.state(yc-1)(xc) = cc.copy(bottom=true)
          case _ =>
        }
      case _ =>
        println((cell, x, y, xBorder, yBorder, xc, yc))
    }
    // TODO update adjacent cell
    val newCell = board.state(yc)(xc)
    if (newCell.fullBorder) {
      board.state(yc)(xc) = FullCell(User(Ansi.orange))
    }
    term.restorePos
    term.savePos
    term.pos(1, 1)
    board.printBoard
    term.restorePos
  }


  for {
    i <- 0 to 200
  } {
    reader.readBinding(map) match {
      case f : Function0[Unit] => f.apply()
      case _ =>
    }
  }

  println(board.state)

  println(Ansi.resetForegroundColor)
  println(Ansi.resetBackgroundColor)

  term.eraseDisplay(2)

  terminal.reset()

}
