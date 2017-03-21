package pbt

object Game {

  type Log = Seq[Board]
  type Game = (Players, Board) => (Winners, Log)

  type Players = Seq[Player]
  type Winners = Set[Player]

  type Strategy = Board => Move

  object Strategies {
    def select(filters: Seq[Cell=>Boolean]): Strategy = board => {
      filters.foldLeft(None: Option[Move]) { case (result, nextTry) =>
        result.orElse {
          board.filter(nextTry) match {
            case Nil => None
            case nonempty =>
              val (x, y, cell) = random(nonempty)
              Some(Move(x, y, random(Edges.all.diff(cell.edges))))
          }
        }
      }.getOrElse(randomStrategy(board))
    }
    val randomStrategy: Strategy = Strategies.select(Seq(_.edges.size<4))
    val avoidingStrategy: Strategy = Strategies.select(Seq(_.edges.size < 2))
    val finishingStrategy: Strategy = Strategies.select(Seq(_.edges.size == 3))
    val fullStrategy: Strategy = Strategies.select(Seq(_.edges.size == 3, _.edges.size == 1, _.edges.isEmpty))

    val all = Seq(randomStrategy, avoidingStrategy, finishingStrategy, fullStrategy)
  }

  case class Move(x: Int, y: Int, edge: Edge) extends Point {
    lazy val adjacent: Move = edge match {
      case Top => Move(x, y-1, Bottom)
      case Bottom => Move(x, y+1, Top)
      case Left => Move(x-1, y, Right)
      case Right => Move(x+1, y, Left)
    }
  }

  case class Player(color: Int, strategy: Strategy)

  case class Board(w: Int, h: Int, cells: Seq[Cell]) extends Rectangle {
    def this(w: Int, h: Int, gen: => Cell = new EmptyCell) =
      this(w,h, Vector.fill(h*w)(gen))
    def filter(f: Cell=>Boolean) = cells.zipWithIndex.collect {
      case (c, i) if f(c) => (i%w, i/w, c)
    }
    lazy val gameEnd = filter(_.edges.size == 4).size == cells.size
    lazy val results: Map[Player, Int] = cells.collect {
      case FullCell(owner) => owner
    }.groupBy(identity).mapValues(_.size).withDefaultValue(0)
    lazy val winners: Winners = results.collect {
      case (p, count) if count == results.values.max => p
    }.toSet
    def update(m: Move, p: Player): Board =
      updated(m,p).updated(m.adjacent, p)

    def updated(m: Move, p: Player): Board =
      if (contains(m)) {
        val i = idx(m)
        val cell = cells(i).updated(m,p)
        Board(w, h, cells.updated(i, cell))
      } else this

  }

  sealed trait Cell {
    def edges: Set[Edge]
    def owner: Option[Player]
    def updated(m: Move, p: Player): Cell
  }
  case class EmptyCell(edges: Set[Edge]) extends Cell {
    def this() = this(Set.empty)
    val owner = None
    def updated(m: Move, p: Player): Cell =
      if (edges.contains(m.edge)) this
      else if (edges.size == 3) FullCell(p)
      else EmptyCell(edges + m.edge)
  }
  case class FullCell(player: Player) extends Cell {
    val edges = Edges.all
    val owner = Some(player)
    def updated(m: Move, p: Player): Cell = this
  }

  sealed trait Edge
  case object Top extends Edge
  case object Bottom extends Edge
  case object Left extends Edge
  case object Right extends Edge

  object Edges {
    val all: Set[Edge] = Set(Top, Bottom, Left, Right)
  }

  type Turn = (Players, Board) => (Players, Board)

  def game: Game = (players, board) => game0(players, board, Seq.empty)

  def game0: (Players, Board, Log) => (Winners, Log) =
    (players, board, log) => {
    if (board.gameEnd) (board.winners, log)
    else {
      val (np, nb) = turn(players, board)
      game0(np, nb, log :+ nb)
    }
  }

  def turn: Turn = (players, board) => {
    val p = players.head
    val move = p.strategy(board)
    val nb = board.update(move, p)
    val np = if (nb.results != board.results) players
    else players.tail :+ p
    (np, nb)
  }
}
