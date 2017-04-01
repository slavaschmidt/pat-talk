package pbt

import org.scalacheck._
import org.scalacheck.Prop._

import Game._

object GameSpec extends Properties("Game") {

  val playerGen = for {
    color <- Gen.oneOf(Ansi.colors)
    strategy <- Gen.oneOf(Strategies.all)
  } yield Player(color, strategy)

  val playersGen = for {
    num <- Gen.choose(2, 6)
    players <- Gen.containerOfN[Set, Player](num, playerGen)
  } yield players.toSeq

  def boardGen(min: Int, max: Int) = for {
    w <- Gen.choose(min, max)
    h <- Gen.choose(min, max)
    n <- Gen.chooseNum(0,3)
    cellEdges <- Gen.listOfN(w*h, Gen.containerOfN[Set, Edge](n, Gen.oneOf(Edges.all.toSeq)))
  } yield Board(w,h, cellEdges.map(EmptyCell))

  def emptyBoardGen(min: Int, max: Int) = for {
    w <- Gen.choose(min, max)
    h <- Gen.choose(min, max)
  } yield new Board(w,h)

  def moveGen(b: Board) = for {
    x <- Gen.choose(1, b.w-2)
    y <- Gen.choose(1, b.h-2)
    freeEdge = random(Edges.all.diff(b.cells(y*b.w+x).edges))
  } yield Move(x,y,freeEdge)

  def borderMoveGen(b: Board) = for {
    x <- Gen.choose(0, b.w-1)
    y <- Gen.choose(0, b.h-1)
    edge = random(Edges.all.diff(b.cells(y*b.w+x).edges))
  } yield edge match {
    case Top => Move(x,0,edge)
    case Bottom => Move(x,b.h-1,edge)
    case Left => Move(0,y,edge)
    case Right => Move(b.w-1,y,edge)
  }

  property("Game") = forAll(boardGen(2, 10), playersGen.suchThat(_.size > 1)) { (b: Board, p: Players) =>
    val (winners, log) = game(b, p)
    all(
      "all winners played" |: winners.diff(p.toSet).isEmpty,
      "all cells are taken"|: log.last.filter(_.owner.isEmpty).isEmpty,
      "winners have most cells"|: winners.forall(log.last.results(_) == log.last.results.values.max)
    )
  }

  property("Turn") = forAll(boardGen(2, 20), playersGen.suchThat(_.size > 1)) { (b: Board, p: Players) =>
    val (board, players) = turn(b, p)
    val cellTaken = board.results(p.head) != b.results(p.head)
    classify(cellTaken, "cell taken", "no cell taken") {
      atLeastOne(
         cellTaken ==> ("player keeps turn if cell was taken" |: players == p),
         ! cellTaken ==> ("player loses turn if no cell was taken" |: all (
          players.head == p(1),
          players.last == p.head
        ))
      )
    }
  }

  property("Move") = forAll(emptyBoardGen(4, 20), playerGen) { (b: Board, p: Player) =>
    forAll (moveGen(b)) { m: Move =>
      val board = m(b,p)
      board.cells.diff(b.cells).size == 2
    }
  }

  property("Border Move") = forAll(emptyBoardGen(4, 20), playerGen) { (b: Board, p: Player) =>
    forAll (borderMoveGen(b)) { m: Move =>
      val board = m(b,p)
      board.cells.diff(b.cells).size == 1
    }
  }

  property("Adjacent Move") = forAll(boardGen(3, 20)) { (b: Board) =>
    forAll (moveGen(b)) { m: Move =>
      m == m.adjacent.adjacent
    }
  }

  property("Higher-ranked strategy wins") = forAllNoShrink(playersGen, boardGen(5, 20)) { (players: Players, board: Board) =>
    val (winners, _) = game(board, players)
    val winningStrategy = winners.headOption.map(_.strategy).map(Strategies.all.indexOf).get
    val strategies = players.map(_.strategy).map(Strategies.all.indexOf)
    collect(winningStrategy, strategies.max) {
      winningStrategy + 1 >= strategies.max
    }
  }

}
