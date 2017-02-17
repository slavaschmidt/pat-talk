package pbt

object Demo {

  type Winners = Set[User]
  type Game = (Seq[User], Board) => Winners
  type Strategy = Board => Move

  type Turn = (Board, User) => Either[Board, Winners]

  case class Board private(width: Int, height: Int, cells: Seq[Cell]) {
    def idx(x: Int, y: Int): Option[Int] =
      if (x>=width || y>=height) None else Some(y*width+x)

    def cell(x: Int, y: Int): Option[Cell] = idx(x,y).map(cells)

    def updated(x: Int, y: Int, cell: Cell): Option[Board] =
      idx(x,y).map(i => this.copy(cells = cells.updated(i, cell)))

    def emptyCells: Seq[EmptyCell] = cells.collect {
      case e: EmptyCell => e
    }
    def fullCells: Seq[Cell] = cells.diff(emptyCells)
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

  sealed trait Cell {
    def borders: Set[Border]
    def emptyBorders: Set[Border] = Border.all.diff(borders)
    def user: Option[User]
  }
  case class FullCell(user: Some[User]) extends Cell{
    def borders: Set[Border] = Border.all
  }
  case class EmptyCell(borders: Set[Border]) extends Cell {
    def user = None
  }



  case class Move(cell: Cell, border: Border)

  case class User(color: Int, strategy: Strategy)

}
