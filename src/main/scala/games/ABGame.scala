package games
import games.Players

class ABGame(var board: List[Player] = List(Players.Player1, Players.None, Players.None, Players.Player2), var currentPlayer: Player=Players.Player1) extends State {
  override def getMoves: List[Move] = {
    val playerIndex = board.indexOf(currentPlayer)
    val leftIndex = if playerIndex == 0 then -1 else if board(playerIndex - 1) == Players.None then playerIndex - 1 else playerIndex - 2
    val rightIndex = if playerIndex == board.length - 1 then -1 else if board(playerIndex + 1) == Players.None then playerIndex + 1 else playerIndex + 2
    List(new Move(leftIndex, 0), new Move(rightIndex, 0)).filter(m => m.x >= 0 && m.x < board.size && board(m.x) == Players.None)
  }

  override def makeMove(move: Move): Unit = {
    require(getMoves.exists(_.x == move.x), "illegal AB move")
    board = board.updated(board.indexOf(currentPlayer), Players.None)
    board = board.updated(move.x, currentPlayer)
    currentPlayer = if (currentPlayer == Players.Player1) Players.Player2 else Players.Player1
  }

  override def hasWinner: Boolean = {
    board.indexOf(Players.Player1) == board.length - 1 || board.indexOf(Players.Player2) == 0
  }

  override def getWinner: Player = {
    (board.indexOf(Players.Player1), board.indexOf(Players.Player2)) match {
      case (i, _) if i == board.length - 1 => Players.Player1
      case (_, i) if i == 0 => Players.Player2
      case _ => Players.None
    }
  }

  override def copy: State = new ABGame(board, currentPlayer)

  override def toString: String = board.mkString(" ")
}
