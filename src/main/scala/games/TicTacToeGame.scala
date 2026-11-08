package games
import games.Players
import utils.padBinaryString

// Space values
// 1   2    4
// 8   16  32
// 64 128 256

//Space names
// 1 2 3
// 4 5 6
// 7 8 9

class TicTacToeBoard(val player1Pieces: Int = 0, val player2Pieces: Int = 0, val playerOneNext: Boolean = true) extends TwoPlayerGame{

    // The length of one row / column
    val boardLength = 3;
    // The total number of spaces on the board
    val boardSize = boardLength * boardLength;

    /**
     * If the game is not over, will return None.
     *
     * If someone has won, it will return Option[true]
     * If the game is over, and it's a draw, it will return Option[false].
     * @return Option[Boolean]
     */
    def hasWinner: Option[Boolean] = {

        val horizontal = for(x <- 1 to boardLength) yield (x to boardSize by boardLength).map(getBoardSpace).sum
        val vertical = for (x <- 0 until boardLength) yield (1 + x*boardLength to boardLength + x*boardLength).map(getBoardSpace).sum

        val forwardDiag = (1 to boardSize by boardLength + 1).map(getBoardSpace).sum
        val backwardDiag = (boardLength to 1 by -1).map(x => x + (boardLength - x) * boardLength).map(getBoardSpace).sum

        val masks = (horizontal concat vertical).toList :+ forwardDiag :+ backwardDiag
        if (masks.exists(mask => (player1Pieces & mask) == mask || (player2Pieces & mask) == mask)) Some(true)
        else if ((player1Pieces | player2Pieces) == (1 << boardSize) - 1) Some(false)
        else None
    }

    /**
     * If player one has won, it will return 1
     * If player two has won, it will return 2.
     * @return Int of the corresponding player
     * @throws IllegalAccessError If the game is not over or a draw
     */
    def getWinner: Int = {
        if(hasWinner.isEmpty || !hasWinner.get){
            throw IllegalAccessError("The game is not over or it's a draw")
        }
        if(!playerOneNext) then 1 else 2
    }

    /**
     * @return true if player 1 is next else false
     */
    def getNextPlayer: Boolean = {
        playerOneNext
    }

    /**
     * @param space The square from 1-boardSize which is being picked to play
     * @return If it is a valid move
     */
    def isValidMove(space: Int): Boolean = space > 0 && space <= boardSize && ((player1Pieces + player2Pieces) & getBoardSpace(space)) == 0

    /**
     * @return All the values 1-boardSize which are legal moves
     */
    def getValidMoves: List[Int] = if (hasWinner.isDefined) Nil else (for i <- 1 to boardSize if isValidMove(i) yield i).toList

    /**
     * @param space The move on which the player should play on
     * @return The next board after the move was played
     * @throws IllegalArgumentException Invalid move, space already occupied
     * @throws IllegalStateException Cannot play, Game already over
     */
    def makeMove(space: Int): TicTacToeBoard = {
        if (!isValidMove(space)){
            throw IllegalArgumentException("Invalid move, space already occupied")
        }
        if(hasWinner.isDefined){
            throw IllegalStateException(s"Cannot play, Game already over. ${if(!hasWinner.get) then "It's a draw" else "A player won!"}")
        }

        val boardSpace = getBoardSpace(space)
        if (playerOneNext) {
            new TicTacToeBoard(player1Pieces + boardSpace, player2Pieces, !playerOneNext)
        }
        else {
            new TicTacToeBoard(player1Pieces, player2Pieces + boardSpace, !playerOneNext)
        }
    }

    override def toString: String = {
        val boardTuples = padBinaryString(player1Pieces.toBinaryString, boardSize).zip(padBinaryString(player2Pieces.toBinaryString, boardSize))
        val outStr = boardTuples.map(x => if x._1 == '1' then "x" else (if x._2 == '1' then "O" else "-")).mkString
        outStr.reverse.grouped(boardLength).map(_+"\n").mkString
    }
    
    /**
     *
     * @param readableValue The human readable value of the board, ie. 1-boardSize
     * @return The base 2 value for that space
     */
    private def getBoardSpace(readableValue: Int): Int = Math.pow(2, readableValue - 1).toInt


    def getGameIntroText = {
        "Playing TicTacToe: \n" +
        "Board spaces: \n" +
        "1 2 3\n" +
        "4 5 6\n" +
        "7 8 9\n"
    }
}

class TicTacToeGameState(var currentPlayer: Player=Players.Player1, var board: TicTacToeBoard = new TicTacToeBoard()) extends State {


    override def getMoves: List[Move] = {
        val vm = board.getValidMoves
        //((i-1)//3+1), ((i-1)%3)+1
        val moves = board.getValidMoves.map(p => new Move(((p-1) % board.boardLength) + 1, ((p - 1) / board.boardLength) + 1))
        moves
    }

    override def makeMove(move: Move): Unit = {
        board = board.makeMove((move.y - 1)* board.boardLength + move.x)
        currentPlayer = if(currentPlayer == Players.Player1) Players.Player2 else Players.Player1
    }
    
    override def copy: TicTacToeGameState = {
        new TicTacToeGameState(currentPlayer, new TicTacToeBoard(board.player1Pieces, board.player2Pieces, board.playerOneNext))
        
    }
    
    override def hasWinner: Boolean = {
        board.hasWinner.isDefined
    }
    
    override def getWinner: Player = {
        if(board.hasWinner.isDefined && board.hasWinner.get){
            if (board.getWinner == 1) Players.Player1 else Players.Player2
        }
        else{
            Players.None
        }
    }

    override def toString: String = {
        board.toString
    }
}
