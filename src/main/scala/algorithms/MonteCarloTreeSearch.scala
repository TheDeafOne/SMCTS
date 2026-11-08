package algorithms

import games.*
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.util.Random

def MCTSMove(iterations: Int = 2000, rollouts: Int = 5, seed: Int = 42): TwoPlayerGame => Int = {
  val seeds = new Random(seed)
  game => {
    val board = game match {
      case b: TicTacToeBoard => b
      case _ => throw new IllegalArgumentException("MCTSMove supports TicTacToeBoard")
    }
    val player = if (board.getNextPlayer) Players.Player1 else Players.Player2
    val root = new Node(new TicTacToeGameState(player, board), null, null)
    val (_, move) = new MonteCarloTreeSearch(root, iterations, rollouts, seeds.nextInt()).search()
    (move.y - 1) * board.boardLength + move.x
  }
}

/** UCT search. Node rewards are stored from the player-to-move's perspective. */
class MonteCarloTreeSearch(val root: Node, val maxIterations: Int = 10,
                          val maxRolloutsPerIteration: Int = 10, seed: Int = 42) {
  require(maxIterations > 0, "iterations must be positive")
  require(maxRolloutsPerIteration > 0, "rollouts must be positive")
  protected val random = new Random(seed)

  protected def outcomes(node: Node): Seq[Player] =
    (1 to maxRolloutsPerIteration).map(_ => node.rollout(random))

  def search(start: Node = root): (Node, Move) = {
    require(!start.state.hasWinner && start.state.getMoves.nonEmpty,
      "cannot search a terminal position or a position with no legal moves")
    for (_ <- 0 until maxIterations) {
      var current = start
      while (current.children.nonEmpty && !current.state.hasWinner) {
        val unvisited = current.children.filter(_.visits == 0)
        current = if (unvisited.nonEmpty) unvisited(random.nextInt(unvisited.size))
                  else current.children.maxBy(_.UCB1)
      }
      if ((!current.state.hasWinner && current.visits > 0) || (current == start && current.children.isEmpty)) {
        current.children = current.state.getMoves.map { move =>
          val next = current.state.copy
          next.makeMove(move)
          new Node(next, current, move)
        }
        if (current.children.nonEmpty)
          current = current.children(random.nextInt(current.children.size))
      }
      // Parallel subclasses calculate independent outcomes; counters update serially here.
      outcomes(current).foreach(current.backprop)
    }
    val best = start.children.maxBy(n => (n.visits, n.valueFor(start.state.currentPlayer)))
    (best, best.move)
  }
}

/** Parallelizes independent simulations, not writes to shared tree statistics. */
class ParallelMonteCarloTreeSearch(root: Node, maxIterations: Int = 10,
                                  maxRolloutsPerIteration: Int = 10, seed: Int = 42)
  extends MonteCarloTreeSearch(root, maxIterations, maxRolloutsPerIteration, seed) {
  override protected def outcomes(node: Node): Seq[Player] = {
    val seeds = (1 to maxRolloutsPerIteration).map(_ => random.nextLong())
    seeds.par.map(s => node.rollout(new Random(s))).toList
  }
}

class Node(val state: State, val parent: Node, val move: Move) {
  var children: List[Node] = List()
  var wins: Double = 0
  var visits: Int = 0

  @tailrec final def backprop(playerWon: Player): Unit = {
    visits += 1
    wins += (if (playerWon == state.currentPlayer) 1 else if (playerWon == Players.None) 0.5 else 0)
    if (parent != null) parent.backprop(playerWon)
  }

  def valueFor(player: Player): Double = {
    if (visits == 0) 0.5
    else if (player == state.currentPlayer) wins / visits
    else 1.0 - wins / visits
  }

  def UCB1: Double = {
    if (visits == 0) Double.PositiveInfinity
    else if (parent == null) valueFor(state.currentPlayer)
    else valueFor(parent.state.currentPlayer) + Math.sqrt(2.0 * Math.log(Math.max(1, parent.visits)) / visits)
  }

  def rollout: Player = rollout(new Random(42))

  def rollout(random: Random, maxPlies: Int = 512): Player = {
    val simulated = state.copy
    var plies = 0
    while (!simulated.hasWinner && plies < maxPlies) {
      val moves = simulated.getMoves
      if (moves.isEmpty) return Players.None
      simulated.makeMove(moves(random.nextInt(moves.size)))
      plies += 1
    }
    if (simulated.hasWinner) simulated.getWinner else Players.None
  }

  override def toString: String =
    s"[wins=$wins visits=$visits value=${valueFor(state.currentPlayer)} move=${Option(move)}]"
}
