package pbt
import scala.util.Random

object demo {

  type Color = Int
  type Strategy = Board => Option[Move]
  type Players = Seq[Player]
  type Winners = Set[Player]
  type Log = Seq[Board]
  type Game = (Players, Board) => (Winners, Log)
  val randomStrategy: Board => Option[Move] = board => {
    val filters = Seq[Cell => Boolean](_.borders.size < 4)
    Strategies.select(filters)(board)
  }
  val finishingStrategy: Board => Option[Move] = board => {
    val filters = Seq[Cell => Boolean](_.borders.size == 3, _.borders.size < 4)
    Strategies.select(filters)(board)
  }
  val fullStrategy: Board => Option[Move] = board => {
    val filters = Seq[Cell => Boolean](_.borders.size == 3, _.borders.size == 1, _.borders.size == 0, _.borders.size < 4)
    Strategies.select(filters)(board)
  }
  val allStrategies = Seq(randomStrategy, finishingStrategy, fullStrategy)
  val allBorders: Set[Border] = Set(Top, Bottom, Left, Right)
  val game: Game = (players, board) => Game.nextTurn(players, board, Seq.empty)
  val turn: (Board, Player) => Board = (board, player) => {
    player.strategy.apply(board).fold(board) { move =>
      board.apply(move, player).fold(board) { adjacent =>
        adjacent.apply(move.adjacent, player).getOrElse(adjacent)
      }
    }
  }

  sealed trait Border

  trait Cell {
    def owner: Option[Player]
    def borders: Set[Border]
  }

  case class Move(x: Int, y: Int, border: Border) {
    lazy val adjacent: Move = border match {
      case Top => Move(x, y - 1, Bottom)
      case Bottom => Move(x, y + 1, Top)
      case Left => Move(x - 1, y, Right)
      case Right => Move(x + 1, y, Left)
    }
  }

  case class Player(color: Color, strategy: Strategy)

  case class EmptyCell(borders: Set[Border]) extends Cell {
    val owner = None
  }

  case class FullCell(player: Player) extends Cell {
    val owner = Some(player)
    override def borders = Set(Top, Bottom, Left, Right)
  }

  case class Board(width: Int, height: Int, cells: Seq[Cell]) {
    def fullCells: Seq[(Int, Int, Cell)] = filter(_.borders.size == 4)

    def filter(f: Cell => Boolean): Seq[(Int, Int, Cell)] =
      cells.zipWithIndex.collect {
        case (c,i) if f(c) => (i % width, i /width, c)
      }

    def apply(m: Move, player: Player): Option[Board] = {
      idx(m) map { i =>
        cells(i) match {
          case f : FullCell => this
          case e @ EmptyCell(borders) if (borders + m.border).size == 4 =>
            new Board(width, height, cells.updated(i, FullCell(player)))
          case e @ EmptyCell(borders) =>
            new Board(width, height, cells.updated(i, EmptyCell(borders + m.border)))
        }
      }
    }

    def idx(m: Move): Option[Int] =
      if (m.x >= width || m.y >= height || m.x < 0 || m.y < 0) None else Some(m.y * width + m.x)
  }

  object Strategies {
    def select(filters: Seq[(Cell) => Boolean]): (Board) => Option[Move] = (board: Board) => {
      filters.foldLeft(None: Option[Move]) { case (result, nextTry) =>
        result.orElse {
          board.filter(nextTry) match {
            case Nil => None
            case nonempty =>
              val cell = randomCell(nonempty)
              Some(Move(cell._1, cell._2, randomBorder(cell._3)))
          }
        }
      }
    }

    def randomBorder(cell: Cell) = {
      val borders = allBorders.diff(cell.borders)
      borders.toIndexedSeq(Random.nextInt(borders.size))
    }

    def randomCell(cells: Seq[(Int, Int, Cell)]) =
      cells(Random.nextInt(cells.size))
  }

  case object Top extends Border

  case object Bottom extends Border

  case object Left extends Border

  case object Right extends Border

  object EmptyCell {
    def apply: Cell = new EmptyCell(Set.empty)
  }

  object Board {
    def apply(width: Int, height: Int, cell: Int => Cell): Board = {
      val cells = for {
        i <- 0 until width * height
      } yield cell(i)
      new Board(width, height, cells)
    }
  }

  object Game {

    val nextTurn: (Players, Board, Log) => (Winners, Log) = (players, board, log) => {
      if (board.cells.isEmpty || players.isEmpty) (Set.empty[Player], Seq.empty[Board])
      else if (board.filter(_.owner.isEmpty).isEmpty) (countWinners(board), log.reverse)
      else {
        val player = players.head
        val nextBoard = turn(board, player)
        val nextPlayers = if (nextBoard.fullCells.size > board.fullCells.size) players else players.tail :+ player
        nextTurn(nextPlayers, nextBoard, nextBoard +: log)
      }
    }

    def countWinners(board: Board): Winners = results(board).headOption.map {
      case (_, count) =>  results(board).filter(_._2 == count).map(_._1)
    }.toSeq.flatten.toSet

    def results(board: Board): Seq[(Player, Color)] = board.cells.collect {
      case FullCell(owner) => owner
    }.groupBy(_.color).map { case (color, cells) => cells.head -> cells.length }.toSeq.sortBy(_._2)
  }

}
