package pbt

import java.awt.event.ActionEvent

import jline.TerminalFactory
import jline.console.{ConsoleReader, KeyMap}

trait Cell {
  val topBottom = "─"
  val leftRight = "│"
  val block = "█"
  val shade = "▓"
  val line = leftRight + shade // " "
}

case class EmptyCell(borders: Seq[Boolean]) extends Cell

class Board(width: Int, height: Int) {

  private val state = Vector.fill(width, height)(EmptyCell(Vector.fill(4)(false)))

  val topLine = "┌" + "─┬" * (width-1) + " ┐"

  val midLine = "├" + "─┼" * (width-1) + " ┤"

  val bottomLine = "└" + "─┴" * (width-1) + " ┘"


  def drawLine(row: Int) = for {
    i <- 0 to width  - 1
  } {
    print(state(i)(row).line)
  }

  def drawBoard = {
    println(topLine)
    for {
      row <- 0 to height-2
    } {
      drawLine(row)
      println(" ")
      println(midLine)
    }
    drawLine(height-1)
    println(" ")
    println(bottomLine)
  }
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

  map.bind(IndexedSeq(27, 91, 65).map(_.toChar), () => term.up(1))
  map.bind(IndexedSeq(27, 91, 68).map(_.toChar), () => term.left(1))
  map.bind(IndexedSeq(27, 91, 67).map(_.toChar), () => term.right(1))
  map.bind(IndexedSeq(27, 91, 66).map(_.toChar), () => term.down(1))

  term.eraseDisplay(2)

  new Board(20, 20).drawBoard

  term.pos(5,5)

  for {
    i <- 0 to 10
  } {
    val a = reader.readBinding(map).asInstanceOf[Function0[Unit]]
    a.apply()
  }

  println(Ansi.resetForegroundColor)
  println(Ansi.resetBackgroundColor)

  term.eraseDisplay(2)

  terminal.reset()

}
