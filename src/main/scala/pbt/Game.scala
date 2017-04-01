package pbt

object Game {

    type Game = (Board, Players) => (Winners, Log)
    type Players = Seq[Player]
    type Winners = Set[Player]
    type Log = Seq[Board]

    type Turn = (Board, Players) => (Board, Players)
    type Strategy = Board => Move

    case class Move(x: Int, y: Int, edge: Edge) extends Point {
      def apply(b: Board, p: Player): Board =
        b.update(this, p).update(this.adjacent, p)
      lazy val adjacent: Move = edge match {
        case Top => Move(x, y-1, Bottom)
        case Bottom => Move(x, y+1, Top)
        case Left => Move(x-1, y, Right)
        case Right => Move(x+1, y, Left)
      }
    }

    case class Player(color: Int, strategy: Strategy)

    case class Board(w: Int, h: Int, cells: Seq[Cell]) extends Rectangle {
      def this(w: Int, h: Int, cellF: => Cell = new EmptyCell()) =
        this(w,h, List.fill(w*h)(cellF))

      def filter(f: Cell => Boolean) = cells.zipWithIndex.collect {
        case (c, i) if f(c) => (i % w, i / w, c)
      }

      val results = cells.collect {
        case FullCell(owner) => owner
      }.groupBy(identity).mapValues(_.length).withDefaultValue(0)

      val winners = results.collect {
        case (k,v) if v == results.values.max => k
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
      def update(m: Move, p: Player): Cell
    }

    case class EmptyCell(edges: Set[Edge]) extends Cell {
      def this() = this(Set.empty)
      val owner = None
      def update(m: Move, p: Player) =
        if (edges.contains(m.edge)) this
        else if (edges.size == 3) FullCell(p)
        else EmptyCell(edges + m.edge)
    }

    case class FullCell(player: Player) extends Cell {
      val owner = Some(player)
      val edges = Edges.all
      def update(m: Move, p: Player) = this
    }


  sealed trait Edge
    case object Top extends Edge
    case object Bottom extends Edge
    case object Left extends Edge
    case object Right extends Edge

    object Edges {
      val all: Set[Edge] = Set(Top, Bottom, Left, Right)
    }


    val game: Game = (board, players) => game0(board, players, Seq(board))

    val game0: (Board, Players, Log) => (Winners, Log) = (board, players, log) => {
      if (board.results.values.sum >= board.cells.size) (board.winners, log.reverse)
      else {
        val (nb, np) = turn(board, players)
        game0(nb, np, nb +: log)
      }
    }

    val turn: Turn = (board, players) => {
      val p = players.head
      val move = p.strategy(board)
      if (board.cells(board.idx(move)).edges.contains(move.edge)) {
        new Exception().printStackTrace()
      }
      val nb = move(board, p)
      if (nb.results != board.results) (nb, players) else (nb, players.tail :+ players.head)
    }




  object Strategies {
    def select(filters: Seq[Cell=>Boolean]): Strategy = board => {
      filters.foldLeft(None: Option[Move]) { case (result, nextTry) =>
        result.orElse {
          board.filter(nextTry) match {
            case Nil => None
            case nonempty =>
              val (x, y, cell) = random(nonempty)
              val edges = random(Edges.all.diff(cell.edges))
              Some(Move(x, y, edges))
          }
        }
      }.getOrElse(randomStrategy(board))
    }
    val randomStrategy: Strategy = Strategies.select(Seq(_.edges.size < 4))
    val avoidingStrategy: Strategy = Strategies.select(Seq(_.edges.size < 2))
    val finishingStrategy: Strategy = Strategies.select(Seq(_.edges.size == 3))
    val fullStrategy: Strategy = Strategies.select(Seq(_.edges.size == 3, _.edges.size == 1, _.edges.isEmpty))

    val all = Seq(randomStrategy, avoidingStrategy, finishingStrategy, fullStrategy)
  }

}
