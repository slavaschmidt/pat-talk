package pbt

import org.scalacheck.Prop._
import org.scalacheck.{Cogen, _}
import pbt.demo._

import scala.util.Random

object GameSpecification extends Properties("Game") {

  val player: Gen[Player] = for {
    strategy <- Gen.oneOf(allStrategies)
    color <- Gen.oneOf(Ansi.colors)
  } yield Player(color, strategy)

  val players: Gen[Seq[Player]] = for {
    i <- Gen.chooseNum(2,6)
    strategy <- Gen.oneOf(allStrategies)
    color <- Random.shuffle(Ansi.colors).take(i)
  } yield Player(color, strategy)

  val emptyBoards: Gen[Board] = for {
    width <- Gen.chooseNum(1, 10)
    height <- Gen.chooseNum(1, 10)
  } yield Board(width, height, _ => EmptyCell.apply)

  implicit val arbPlayer = Arbitrary(player)
  implicit val arbBoard = Arbitrary(emptyBoards)

  implicit val b: Cogen[Board] = Cogen(_.hashCode())
  implicit val p: Cogen[Player] = Cogen(_.hashCode())

  val f = Arbitrary.arbitrary[Game]

  property("Game") = forAllNoShrink { (players: Players, board: Board) =>
    val (winners, log) = game(players, board)
    val winningStrategy = winners.headOption.map(_.strategy).map(allStrategies.indexOf)
    val strategies = players.map(_.strategy).map(allStrategies.indexOf)
    // log map SpeakerCheat.printBoard
    all (
      "all winners are players" |: winners.diff(players.toSet).isEmpty == true,
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
      "roundtrip" |: move.adjacent.adjacent == move,
      "not the same border" |: move.adjacent.border != move.border,
      "not the same X or Y" |: ((move.adjacent.x != move.x && move.adjacent.y == move.y) || (move.adjacent.x == move.x && move.adjacent.y != move.y)),
      "X difference is 1" |: Math.abs(move.adjacent.x - move.x) < 2,
      "Y difference is 1" |: Math.abs(move.adjacent.y - move.y) < 2
    )
  }
}
