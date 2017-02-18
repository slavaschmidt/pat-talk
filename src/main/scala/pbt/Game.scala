package pbt

import pbt.Demo.Board

import scala.util.Random

object RunnableDemo extends App {
  val players = Seq(Demo.User(Ansi.green, Demo.finishingStrategy), Demo.User(Ansi.orange, Demo.twinStrategy))
  val board = Board(10, 10, () => Demo.EmptyCell(Set.empty))
  val winners = Demo.game(players, board)
  println(winners)
}

object Demo {

  type Winners = Set[User]
  type Game = (Seq[User], Board) => Winners
  type Strategy = Board => Move

  type Turn = (Board, User) => Board
  case class Move(x: Int, y: Int, border: Border) {
    lazy val adjacent: Move = border match {
      case Up => Move(x, y-1, Down)
      case Down => Move(x, y+1, Up)
      case Left => Move(x-1, y, Right)
      case Right => Move(x+1, y, Left)
    }
  }
  case class User(color: Int, strategy: Strategy)

  def randomStrategy: Strategy = board => {
    val cell = board.emptyCells.apply(Random.nextInt(board.emptyCells.size))
    val (x, y) = board.coords(cell)
    val border = Border.all.diff(cell.borders).head
    Move(x, y, border)
  }
  def finishingStrategy: Strategy = board => {
    val cells = board.emptyCells
    val bestCells = cells.filter(_.borders.size == 3)
    val cell = if (bestCells.nonEmpty) bestCells(Random.nextInt(bestCells.size)) else cells(Random.nextInt(cells.size))
    val (x, y) = board.coords(cell)
    val border = Border.all.diff(cell.borders).head
    Move(x, y, border)
  }
  def twinStrategy: Strategy = board => {
    val cells = board.emptyCells
    val bestCells = cells.filter(_.borders.size == 3)
    lazy val nextBestCells = cells.filter(_.borders.size == 1)
    val cell =
      if (bestCells.nonEmpty) bestCells(Random.nextInt(bestCells.size))
      else if (nextBestCells.nonEmpty) nextBestCells(Random.nextInt(nextBestCells.size))
      else cells(Random.nextInt(cells.size))
    val (x, y) = board.coords(cell)
    val border = Border.all.diff(cell.borders).head
    Move(x, y, border)
  }
  // @tailrec
  def game: Game = (users, board) => {
    val nextUser = users.head
    val newBoard = turn(board, nextUser)
    SpeakerCheat.drawBoard(newBoard)
    if (newBoard.gameEnd)
      newBoard.winners
    else {
      val nextUsers =
        if (newBoard.fullCells.size > board.fullCells.size) users
        else users.tail :+ nextUser
      Thread.sleep(100)
      game(nextUsers, newBoard)
    }
  }

  def turn: Turn = (b, user) => {
    val m = user.strategy(b)

    def updateBoard(board: Board, move: Move, user: User): Board = {
      val newCell = board.cell(move.x, move.y).map(_.withBorder(move.border, user))
      newCell.flatMap(board.updated(move.x, move.y)).getOrElse(board)
    }

    updateBoard(updateBoard(b, m, user), m.adjacent, user)
  }

  case class Board private(width: Int, height: Int, cells: Seq[Cell]) {
    private def idx(x: Int, y: Int): Option[Int] =
      if (x>=width || y>=height || x<0 || y<0) None else Some(y*width+x)

    def cell(x: Int, y: Int): Option[Cell] = idx(x,y).map(cells)

    def updated(x: Int, y: Int)(cell: Cell): Option[Board] =
      idx(x,y).map(i => this.copy(cells = cells.updated(i, cell)))

    val emptyCells: Seq[EmptyCell] = cells.collect {
      case e: EmptyCell => e
    }
    def coords(cell: Cell): (Int, Int) = {
      val i = cells.indexWhere(_.eq(cell))
      (i % width, i / width)
    }
    val fullCells: Seq[Cell] = cells.diff(emptyCells)
    val gameEnd: Boolean = emptyCells.isEmpty
    lazy val results: Seq[(User, Int)] = cells.collect {
      case FullCell(Some(user)) => user
    }.groupBy(_.color).map(p => p._2.head -> p._2.length).toSeq.sortBy(_._2)
    lazy val winners: Set[User] = results.headOption.map {
      case (_, count) =>  results.filter(_._2 == count).map(_._1)
    }.toSeq.flatten.toSet
    def row(y: Int): Seq[Cell] = cells.slice(y * width, (y+1) * width)
  }

  object Board {
    def apply(width: Int, height: Int, defaultCell: () => Cell) =
      new Board(width, height, Vector.fill(width*height)(defaultCell()))
  }

  sealed trait Border
  case object Up extends Border
  case object Down extends Border
  case object Left extends Border
  case object Right extends Border
  object Border {
    def all: Set[Border] = Set(Up, Down, Left, Right)
  }

  trait Cell {
    def borders: Set[Border]
    def emptyBorders: Set[Border] = Border.all.diff(borders)
    def user: Option[User]
    def withBorder(b: Border, user: User): Cell
  }
  case class FullCell(user: Some[User]) extends Cell{
    def borders: Set[Border] = Border.all
    def withBorder(b: Border, user: User): FullCell = this
  }
  case class EmptyCell(borders: Set[Border]) extends Cell {
    def user = None
    def withBorder(b: Border, user: User): Cell =
      if (borders.contains(b)) this
      else if (borders.size == 3) FullCell(Some(user))
      else copy(borders + b)
  }
}
