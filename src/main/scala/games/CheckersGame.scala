package games

import games.Players.{Player1, Player2}

/** English-style checkers: compulsory captures, chosen multi-jumps, crown ends the turn. */
class CheckersGame(var currentPlayer: Player = Player1) extends State {
  var board: List[List[Player]] = (0 until 8).map { row =>
    (0 until 8).map { col =>
      if ((row + col) % 2 != 0) Players.None
      else if (row < 3) Player1
      else if (row > 4) Player2
      else Players.None
    }.toList
  }.toList
  // Promotion belongs to a square in this position, never to the global player singleton.
  var kings: Set[(Int, Int)] = Set.empty
  private var continuingFrom: Option[(Int, Int)] = None

  private def oppositePlayer: Player = if (currentPlayer == Player1) Player2 else Player1
  def getPiece(x: Int, y: Int): Player = board(x)(y)
  def inBounds(pos: Int): Boolean = pos >= 0 && pos < 8

  private def candidateMoves(x: Int, y: Int, capturesOnly: Boolean): List[Move] = {
    val directions = if (kings((x, y))) List(-1, 1)
                     else if (currentPlayer == Player1) List(1) else List(-1)
    directions.flatMap { dx =>
      List(-1, 1).flatMap { dy =>
        val nx = x + dx
        val ny = y + dy
        if (!inBounds(nx) || !inBounds(ny)) None
        else if (getPiece(nx, ny) == Players.None && !capturesOnly)
          Some(new Move(nx, ny, x, y))
        else if (getPiece(nx, ny) == oppositePlayer &&
                 inBounds(x + 2 * dx) && inBounds(y + 2 * dy) &&
                 getPiece(x + 2 * dx, y + 2 * dy) == Players.None)
          Some(new Move(x + 2 * dx, y + 2 * dy, x, y))
        else None
      }
    }
  }

  def getMove(x: Int, y: Int): List[Move] =
    if (getPiece(x, y) == currentPlayer) candidateMoves(x, y, false) else Nil

  override def getMoves: List[Move] = {
    val positions = continuingFrom.toList match {
      case Nil => (for (x <- 0 until 8; y <- 0 until 8 if getPiece(x, y) == currentPlayer) yield (x, y)).toList
      case xs => xs
    }
    val captures = positions.flatMap((x, y) => candidateMoves(x, y, true))
    if (captures.nonEmpty) captures
    else if (continuingFrom.nonEmpty) Nil
    else positions.flatMap((x, y) => candidateMoves(x, y, false))
  }

  def updateSpot(x: Int, y: Int, player: Player): List[List[Player]] =
    board.updated(x, board(x).updated(y, player))

  override def makeMove(move: Move): Unit = {
    require(getMoves.exists(m => m.x == move.x && m.y == move.y &&
      m.currX == move.currX && m.currY == move.currY), "illegal checkers move")
    val wasKing = kings((move.currX, move.currY))
    board = updateSpot(move.currX, move.currY, Players.None)
    kings -= ((move.currX, move.currY))
    val captured = Math.abs(move.x - move.currX) == 2
    if (captured) {
      val middle = ((move.x + move.currX) / 2, (move.y + move.currY) / 2)
      board = updateSpot(middle._1, middle._2, Players.None)
      kings -= middle
    }
    board = updateSpot(move.x, move.y, currentPlayer)
    val promoted = !wasKing && (if (currentPlayer == Player1) move.x == 7 else move.x == 0)
    if (wasKing || promoted) kings += ((move.x, move.y))
    if (captured && !promoted && candidateMoves(move.x, move.y, true).nonEmpty)
      continuingFrom = Some((move.x, move.y))
    else {
      continuingFrom = None
      currentPlayer = oppositePlayer
    }
  }

  override def hasWinner: Boolean = getMoves.isEmpty
  override def getWinner: Player = if (hasWinner) oppositePlayer else Players.None

  override def copy: CheckersGame = {
    val result = new CheckersGame(currentPlayer)
    result.board = board
    result.kings = kings
    result.continuingFrom = continuingFrom
    result
  }

  override def toString: String =
    board.zipWithIndex.map((row, x) => row.zipWithIndex.map((p, y) =>
      if (p == Players.None) "." else if (p == Player1) { if (kings((x,y))) "X" else "x" }
      else { if (kings((x,y))) "O" else "o" }).mkString(" ")).mkString("\n")
}
