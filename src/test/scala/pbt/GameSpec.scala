package pbt

import org.scalacheck.Prop._
import org.scalacheck._
import pbt.Game._

object GameSpec extends Properties("Heisenbug") {

  implicit val playerGen: Gen[Player] = for {
    color <- Gen.oneOf(Ansi.colors)
    strategy <- Gen.oneOf(Strategies.all)
  } yield Player(color, strategy)

  implicit val playersGen: Gen[Players] = for {
    num <- Gen.chooseNum(2, 6)
    player <- Gen.listOfN(num, playerGen)
  } yield player.distinct

  implicit val boardsGen: Gen[Board] = for {
    width <- Gen.choose(1,20)
    height <- Gen.choose(1,20)
    n <- Gen.chooseNum(0,3)
    border <- Gen.listOfN(n, Gen.oneOf(Edges.all.toSeq))
  } yield new Board(width, height, EmptyCell(border.toSet))

  implicit def emptyBoardsGen(low: Int = 1, high: Int = 20): Gen[Board] = for {
    width <- Gen.choose(low, high)
    height <- Gen.choose(low, high)
  } yield new Board(width, height)

  implicit def borderMove(board: Board): Gen[Move] = {
    val right = board.w-1
    val bottom = board.h-1
    for {
      x <- Gen.choose(0, right)
      y <- Gen.choose(0, bottom)
      edge <- Gen.oneOf(Edges.all.toSeq)
    } yield edge match {
      case Top => Move(x, 0, edge)
      case Bottom => Move(x, bottom, edge)
      case Left => Move(0, y, edge)
      case Right => Move(right, y, edge)
    }
  }

  implicit def moveGen(board: Board): Gen[Move] = for {
    x <- Gen.oneOf(1, board.w-2)
    y <- Gen.oneOf(1, board.h-2)
    edge <- Gen.oneOf(Edges.all.toSeq)
  } yield Move(x, y, edge)

  property("Game") = forAll(playersGen, emptyBoardsGen()) { (players: Players, board: Board) =>
    val (winners, log) = game(players, board)
    // log map SpeakerCheat.printBoard
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
          board.results != b.results ==> ("The player has a next turn if it took a cell" |: (players.head == p.head) && boardDiff.forall(_.owner.isDefined))
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
      board.cells.diff(board.update(move, player).cells).size == 1
    }
  }

  implicit val playerArb = Arbitrary(playerGen)
  implicit val boardArb = Arbitrary(emptyBoardsGen(3,20))

  property("Move changes two cells") = forAll { (player: Player, board: Board) =>
    forAll(moveGen(board)) { (move: Move) =>
      board.cells.diff(board.update(move, player).cells).size == 2
    }
  }

  property("Higher-ranked strategy wins") = forAllNoShrink(playersGen, emptyBoardsGen(5, 20)) { (players: Players, board: Board) =>
    val (winners, _) = game(players, board)
    val winningStrategy = winners.headOption.map(_.strategy).map(Strategies.all.indexOf).get
    val strategies = players.map(_.strategy).map(Strategies.all.indexOf)
    collect(winningStrategy, strategies.max) {
      winningStrategy + 1 >= strategies.max
    }
  }

}