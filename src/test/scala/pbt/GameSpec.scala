package pbt

import org.scalacheck.Prop._
import org.scalacheck._
import pbt.Demo._

object GameSpec extends Properties("Game") {

  implicit val playerGen: Gen[Player] = for {
    color <- Gen.oneOf(Ansi.colors)
    strategy <- Gen.oneOf(allStrategies)
  } yield Player(color, strategy)

  implicit val playersGen: Gen[Players] = for {
    num <- Gen.chooseNum(2, 6)
    player <- Gen.listOfN(num, playerGen)
  } yield player.distinct

  implicit val boardsGen: Gen[Board] = for {
    width <- Gen.choose(1,20)
    height <- Gen.choose(1,20)
    n <- Gen.chooseNum(0,3)
    border <- Gen.listOfN(n, Gen.oneOf(allBorders.toSeq))
  } yield Board(width, height, (i: Int) => EmptyCell(border.toSet))

  implicit def emptyBoardsGen(low: Int = 1, high: Int = 20): Gen[Board] = for {
    width <- Gen.choose(low, high)
    height <- Gen.choose(low, high)
  } yield Board(width, height, (i: Int) => new EmptyCell())

  implicit def borderMove(board: Board): Gen[Move] = for {
    x <- Gen.oneOf(0, board.width-1)
    y <- Gen.oneOf(0, board.height-1)
    border = (x,y) match {
      case (0, _) => Left
      case (xx, _) if xx == board.width-1 => Right
      case (_, 0) => Top
      case (_, yy) if yy == board.height-1 => Bottom

    }
  } yield Move(x, y, border)

  implicit def moveGen(board: Board): Gen[Move] = for {
    x <- Gen.oneOf(1, board.width-2)
    y <- Gen.oneOf(1, board.height-2)
    border <- Gen.oneOf(allBorders.toSeq)
  } yield Move(x, y, border)

  property("Game") = forAll(playersGen, emptyBoardsGen()) { (players: Players, board: Board) =>
    val (winners, log) = game(players, board)
    all(
      "all winners are players" |: winners.diff(players.toSet).isEmpty,
      "One or two changed cells" |: log.sliding(2).forall { case Seq(before, after) =>
        val boardDiff = after.cells.diff(before.cells)
        boardDiff.size == 1 || boardDiff.size == 2
      }
    )
  }

  property("Turn Players") = forAllNoShrink(playersGen, boardsGen) { (p: Players, b: Board) =>
    val (players, board) = turn(p, b)
    val boardDiff = board.cells.diff(b.cells)
    all (
      "No new players" |: players.toSet.diff(p.toSet).isEmpty,
      classify(board.results == b.results, "no cell taken", "cell taken") {
        atLeastOne (
          board.results == b.results ==> ("The player is last in the queue if no cell was taken" |: { players.last.color == p.head.color }),
          board.results != b.results ==> ("The player has a next turn if it took a cell" |: (players.head == p.head) && boardDiff.forall(_.full))
        )
      }
    )
  }

  property("Turn Board") = forAllNoShrink(playersGen, boardsGen) { (p: Players, b: Board) =>
    val (players, board) = turn(p, b)
    val boardDiff = board.cells.diff(b.cells)
    "One or two changed cells" |: atLeastOne(boardDiff.size == 1, boardDiff.size == 2)
  }

  property("Move on the border changes one cell") = forAll(playerGen, emptyBoardsGen()) { (player: Player, board: Board) =>
    forAll(borderMove(board)) { (move: Move) =>
      board.cells.diff(board.update(move, player).update(move.adjacent, player).cells).size == 1
    }
  }

  implicit val playerArb = Arbitrary(playerGen)
  implicit val boardArb = Arbitrary(emptyBoardsGen(3,20))

  property("Move changes two cells") = forAll { (player: Player, board: Board) =>
    forAll(moveGen(board)) { (move: Move) =>
      board.cells.diff(board.update(move, player).update(move.adjacent, player).cells).size == 2
    }
  }

  property("Strategies are different") = forAllNoShrink(playersGen, emptyBoardsGen(5, 20)) { (players: Players, board: Board) =>
    val (winners, _) = game(players, board)
    val winningStrategy = winners.headOption.map(_.strategy).map(allStrategies.indexOf)
    val strategies = players.map(_.strategy).map(allStrategies.indexOf)
    collect(winningStrategy, strategies.max) {
      winningStrategy.forall(_ + 1 >= strategies.max)
    }
  }

}
