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
    num <- Gen.choose(2,6)
    players <- Gen.listOfN(num, playerGen)
  } yield players

  def emptyBoardGen(min: Int, max: Int) = for {
    w <- Gen.choose(min, max)
    h <- Gen.choose(min, max)
  } yield new Board(w,h)

  def boardGen(min: Int, max: Int) = for {
    w <- Gen.choose(min, max)
    h <- Gen.choose(min, max)
    num <- Gen.choose(0,3)
    edges <- Gen.listOfN(w*h, Gen.containerOfN[Set, Edge](num, Gen.oneOf(Edges.all.toSeq)))
  } yield Board(w,h, edges.map(EmptyCell))

  def moveGen(b: Board) = for {
    w <- Gen.choose(0, b.w)
    h <- Gen.choose(0, b.h)
    edge <- Gen.oneOf(Edges.all.toSeq)
  } yield Move(w,h, edge)

  property("Game") = forAll(emptyBoardGen(2,20), playersGen.suchThat(_.size>2)) { (b: Board, p: Players) =>
    val (winners, log) = game(b, p)
    log map SpeakerCheat.printBoard
    all(
      "all winners played a game" |: winners.diff(p.toSet).isEmpty,
      "no more empty cells" |: log.last.cells.filter(_.isEmpty).isEmpty
    )
  }

  property("Turn") = forAll(boardGen(2,20), playersGen) { (b: Board, p: Players) =>
    val (nb, np) = turn(b, p)
    val cellTaken = nb.results != b.results
    all(
      "player made a move" |: nb != b,
      "no more then two cells are changed" |: nb.cells.diff(b.cells).size < 3,
      classify(cellTaken, "Cell taken", "No cell taken") {
        atLeastOne(
          "player keeps the turn if cell was taken" |: cellTaken ==> (p == np),
          "player changes if no cell was taken" |: !cellTaken ==> (p(1) == np.head && np.last == p.head)
        )
      }
    )
  }

  property("Move") = forAll(emptyBoardGen(2,20)) { (b: Board) =>
    forAll(moveGen(b)) { (m: Move) =>
      m.adjacent.adjacent == m
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
