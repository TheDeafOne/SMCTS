import algorithms.{MonteCarloTreeSearch, Node}
import games.{ABGame, CheckersGame, TicTacToeGameState}

@main def tester(): Unit = {
  val root = new Node(new TicTacToeGameState(), null, null)
  val MCTS = new MonteCarloTreeSearch(root, 100)
  var current = root
  while (!current.state.hasWinner) {
    val (node, move) = MCTS.search(current)
    println(s"Children: ${current.children.map(c => s"${c.move} - ${c.wins/c.visits}").mkString(" ")}")
    current = node
  }
  println(current.state)
}