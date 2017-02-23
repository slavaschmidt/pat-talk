package pbt

object Demo {

  type Game = (Players, Board) => (Winners, Log)

  type Players = Seq[Player]
  type Winners = Set[Player]

  case class Player(color: Color, strategy: Strategy)

  type Strategy = Board => Move

  type Color = Int

  type Log = Seq[Board]

  type Turn = (Players, Board) => (Players, Board)

  type CellFilter = Cell => Boolean

  def game: Game = (players, board) => game0(players, board, Seq.empty)

  def game0: (Players, Board, Log) => (Winners, Log) = (players, board, log) => {
    if (board.filter(_.notFull).isEmpty) (board.winners, log.reverse)
    else {
      val (newPlayers, newBoard) = turn(players, board)
      game0(newPlayers, newBoard, newBoard +: log)
    }
  }

  def turn: Turn = (players, board) => {
    val player = players.head
    val move = player.strategy(board)
    val newBoard = board.update(move, player).update(move.adjacent, player)
    if (newBoard.results == board.results) (players.tail :+ player, newBoard)
    else (players, newBoard)
  }

  case class Move(x: Int, y: Int, border: Border) extends Point {
    lazy val adjacent: Move = border match {
      case Top => Move(x, y - 1, Bottom)
      case Bottom => Move(x, y + 1, Top)
      case Left => Move(x - 1, y, Right)
      case Right => Move(x + 1, y, Left)
    }
  }

  sealed trait Border
  case object Left extends Border
  case object Right extends Border
  case object Bottom extends Border
  case object Top extends Border

  val allBorders: Set[Border] = Set(Left, Right, Bottom, Top)

  sealed trait Cell {
    def borders: Set[Border]
    def owner: Option[Player]
    lazy val full: Boolean = borders.size == 4
    lazy val notFull: Boolean = !full
    def update(b: Border, p: Player): Cell
  }

  case class EmptyCell(borders: Set[Border]) extends Cell {
    def this() = this(Set.empty)
    val owner = None

    override def update(b: Border, p: Player): Cell =
      if (borders.contains(b)) this
      else if (borders.size == 3) FullCell(p)
      else EmptyCell(borders + b)
  }

  case class FullCell(player: Player) extends Cell {
    val borders: Set[Border] = Set(Top, Bottom, Left, Right)
    val owner = Some(player)
    override def update(b: Border, p: Player): FullCell = this
  }

  val randomStrategy: Strategy = Strategies.select(Seq(_.notFull))
  val avoidingStrategy: Strategy = Strategies.select(Seq(_.borders.size < 2))
  val finishingStrategy: Strategy = Strategies.select(Seq(_.borders.size == 3))
  val fullStrategy: Strategy = Strategies.select(Seq(_.borders.size == 3, _.borders.size == 1, _.borders.isEmpty))

  object Strategies {
    def select(filters: Seq[CellFilter]): Strategy = board => {
      filters.foldLeft(None: Option[Move]) { case (result, nextTry) =>
        result.orElse {
          board.filter(nextTry) match {
            case Nil => None
            case nonempty =>
              val (x, y, cell) = random(nonempty)
              Some(Move(x, y, random(allBorders.diff(cell.borders))))
          }
        }
      }.getOrElse(randomStrategy(board))
    }
  }

  val allStrategies = Seq(randomStrategy, avoidingStrategy, finishingStrategy, fullStrategy)

  case class Board(width: Int, height: Int, cells: Seq[Cell]) extends Rectangle {
    def this(width: Int, height: Int, gen: => Cell = new EmptyCell()) =
      this(width, height, Vector.fill(width * height)(gen))

    def filter(f: Cell => Boolean): Seq[(Int, Int, Cell)] =
      cells.zipWithIndex.collect {
        case (c, i) if f(c) => (i % width, i / width, c)
      }

    def update(move: Move, player: Player): Board =
      if (contains(move)) {
        val i = idx(move)
        val cell = cells(i)
        Board(width, height, cells.updated(i, cell.update(move.border, player)))
      }
      else this

    lazy val results: Map[Player, Int] = cells.collect {
      case FullCell(owner) => owner
    }.groupBy(identity).mapValues(_.size)

    lazy val winners: Winners = results.filter {
      case (_, count) => count == results.values.max
    }.keySet
  }
}
