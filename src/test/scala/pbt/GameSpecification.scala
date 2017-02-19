package pbt

import org.scalacheck._
import org.scalacheck.Prop._
import pbt.demo._

import scala.util.Random

object GameSpecification extends Properties("Game") {

  val players: Gen[Seq[Player]] = for {
    i <- Gen.chooseNum(2,6)
    strategy <- Gen.oneOf(allStrategies)
    color <- Random.shuffle(Ansi.colors).toSeq.take(i)
  } yield Player(color, strategy)

  val emptyBoards: Gen[Board] = for {
    width <- Gen.chooseNum(1, 20)
    height <- Gen.chooseNum(1, 20)
  } yield Board(width, height, _ => EmptyCell.apply)

  property("Game") = forAllNoShrink(players, emptyBoards) { (players: Players, board: Board) =>
    val (winners, log) = Game.apply(players, board)
    val winningStrategy = winners.headOption.map(_.strategy).map(allStrategies.indexOf)
    val strategies = players.map(_.strategy).map(allStrategies.indexOf)
    // log map SpeakerCheat.printBoard
    all (
      "all winners are players" |:winners.diff(players.toSet).isEmpty == true,
      "have winners for nonempty body" |: board.cells.nonEmpty == winners.nonEmpty,
      "have no winners for empty body" |: board.cells.isEmpty == winners.isEmpty,
      "have no more winners than cells" |: board.cells.size >= winners.size,
      "winning strategy is correct" |: winningStrategy.forall(_ == strategies.max)
    )
  }

  val moves = for {
    x <- Gen.posNum[Int]
    y <- Gen.posNum[Int]
    border <- Gen.oneOf(allBorders.toSeq)
  } yield Move(x, y, border)

  property("adjacent move") = forAll(moves) { move: Move =>
    all(
      "roundtrip" |: move.adjacent.adjacent == move
    )
  }
}
