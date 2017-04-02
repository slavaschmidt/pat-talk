package pbt

object Game {

    type Game = (Board, Players) => (Winners, Log)
    type Players = Seq[Player]
    type Winners = Set[Player]
    type Log = Seq[Board]

    type Turn = (Board, Players) => (Board, Players)
    type Strategy = Board => Move

    case class Player(color: Int, strategy: Strategy)

    case class Move(x: Int, y: Int, edge: Edge) extends Point {
      def apply(b: Board, p: Player) = {
        b.update(this, p).update(this.adjacent, p)
      }

      lazy val adjacent: Move = edge match {
        case Top => Move(x, y-1, Bottom)
        case Bottom => Move(x, y+1, Top)
        case Left => Move(x-1, y, Right)
        case Right => Move(x+1, y, Left)
      }
    }

    case class Board(w: Int, h: Int, cells: Seq[Cell]) extends Rectangle {
      def this(w: Int, h: Int, cellF: => Cell = new EmptyCell) = this(w,h,List.fill(w*h)(cellF))
      def filter(f: Cell => Boolean) = cells.zipWithIndex.collect {
        case (c, i) if f(c) => (i%w, i/w, c)
      }
      lazy val results = cells.collect {
        case FullCell(p) => p
      }.groupBy(identity).mapValues(_.length)
      lazy val gameEnd = results.values.sum == cells.size
      lazy val winners = results.collect {
        case (p, count) if count == results.values.max => p
      }.toSet
      def update(m: Move, p: Player) = {
        if (contains(m)) {
          val cell = cells(idx(m)).update(m, p)
          Board(w,h,cells.updated(idx(m), cell))
        } else this
      }
    }

    sealed trait Cell {
      def owner: Option[Player]
      def edges: Set[Edge]
      def isEmpty: Boolean
      def update(m: Move, p: Player): Cell
    }

    case class EmptyCell(edges: Set[Edge]) extends Cell {
      def this() = this(Set.empty)
      val owner = None
      val isEmpty = true
      def update(m: Move, p: Player): Cell =
        if (edges.contains(m.edge)) this
        else if (edges.size == 3) FullCell(p)
        else EmptyCell(edges + m.edge)
    }
    case class FullCell(player: Player) extends Cell {
      val owner = Option(player)
      val edges = Edges.all
      val isEmpty = false
      def update(m: Move, p: Player): Cell = this
    }

    sealed trait Edge
    case object Top extends Edge
    case object Bottom extends Edge
    case object Left extends Edge
    case object Right extends Edge

    object Edges {
      val all: Set[Edge] = Set(Top, Bottom, Left, Right)
    }

    def game: Game = (board, players) => game0(board, players, Seq(board))

    def game0: (Board, Players, Log) => (Winners, Log) = (board, players, log) => {
      if (board.gameEnd) (board.winners, log)
      else {
        val (nb, np) = turn(board, players)
        game0(nb, np, log :+ nb)
      }
    }

    def turn: Turn = (board, players) => {
      val p = players.head
      val move = p.strategy(board)
      val nb = move(board, p)
      if (nb.results != board.results) (nb, players) else (nb, players.tail :+ p)
    }

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


}
