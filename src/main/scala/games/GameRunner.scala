package games

import java.util.Scanner
import scala.util.control.Breaks.break
import algorithms.{MCTSMove, getSimpleTTTBHeuristic, greedyTTTMove, humanMove, minimaxTTTMove, parallelGreedyTTTMove, parallelMinimaxTTTMove, randomMove}
import utils.timeIt

@main
def run = {
  //    runGame(humanMove, humanMove, verbose = true, TicTacToeBoard())
  //    runGame(humanMove, randomMove, verbose = true, TicTacToeBoard())
  //    runGame(humanMove, greedyTTTMove(), verbose = true, TicTacToeBoard())
//  runGame(humanMove, minimaxTTTMove(10), verbose = true, TicTacToeBoard())
//    val result = (1 to 100).map(x => runGame(greedyTTTMove(), randomMove, verbose = false, TicTacToeBoard()))
    val result = (1 to 100).map(x => runGame(minimaxTTTMove(10), randomMove, verbose = false, TicTacToeBoard()))
//    val result = (1 to 100).map(x => runGame(randomMove, randomMove, verbose = false, TicTacToeBoard())).toList
    println(s"Player 1 won: ${result.count(_==1)} and drawed: ${result.count(_==(-1))}")
}

@main
def timeRuns = {
  println("random v mcts")
  val b = new TicTacToeBoard()
  val outC = timeIt(runGame(randomMove, MCTSMove(), verbose = false, TicTacToeBoard()))
//    println("Running Random: ")
//    val outR = timeIt(runGame(randomMove, randomMove, verbose = false, TicTacToeBoard()))
//    println(s"Ran in ${outR._1}ms\n")
//
//    println("Running Greedy: ")
//    val outG = timeIt(runGame(minimaxTTTMove(10), minimaxTTTMove(10), verbose = false, TicTacToeBoard()))
//    println(s"Ran in ${outG._1}ms\n")
//    println("Running ParGreedy: ")
//    val outPG = timeIt(runGame(parallelMinimaxTTTMove(10), parallelMinimaxTTTMove(10), verbose = false, TicTacToeBoard()))
//    println(s"Ran in ${outPG._1}ms\n")
//
//    println("Running SimpleMinimax: ")
//    val outSMM = timeIt(runGame(minimaxTTTMove(10, getSimpleTTTBHeuristic), minimaxTTTMove(10, getSimpleTTTBHeuristic), verbose = false, TicTacToeBoard()))
//    println(s"Ran in ${outSMM._1}ms\n")
//    println("Running SimpleParallelMinimax: ")
//    val outPSMM = timeIt(runGame(parallelMinimaxTTTMove(10, getSimpleTTTBHeuristic), parallelMinimaxTTTMove(10, getSimpleTTTBHeuristic), verbose = false, TicTacToeBoard()))
//    println(s"Ran in ${outPSMM._1}ms\n")
//
//    println("Running Minimax: ")
//    val outMM = timeIt(runGame(minimaxTTTMove(10), minimaxTTTMove(10), verbose = false, TicTacToeBoard()))
//    println(s"Ran in ${outMM._1}ms\n")
//    println("Running ParMinimax: ")
//    val outPMM = timeIt(runGame(parallelMinimaxTTTMove(10), parallelMinimaxTTTMove(10), verbose = false, TicTacToeBoard()))
//    println(s"Ran in ${outPMM._1}ms\n")
}


def runGame(player1Algo: TwoPlayerGame => Int, player2Algo: TwoPlayerGame => Int, verbose: Boolean = false, startingBoard: TwoPlayerGame): Int = {
  if (verbose) println(startingBoard.getGameIntroText)
  @scala.annotation.tailrec
  def gameLoop(board: TwoPlayerGame): Int = {
    if (board.hasWinner.isDefined) {
      val result = if (board.hasWinner.get) board.getWinner else -1
      if (verbose) {
        println(if (result == -1) "Game Over. Draw!" else s"Game Over! Player $result")
        println(board)
      }
      result
    } else {
      val move = if (board.getNextPlayer) player1Algo(board) else player2Algo(board)
      if (move == -1) -2
      else {
        require(board.getValidMoves.contains(move), s"algorithm returned illegal move $move")
        gameLoop(board.makeMove(move))
      }
    }
  }
  gameLoop(startingBoard)
}
