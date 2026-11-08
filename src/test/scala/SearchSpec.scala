import algorithms.*
import games.*
import org.scalatest.funsuite.AnyFunSuite

class SearchSpec extends AnyFunSuite {
  def position(moves: Int*): TicTacToeBoard =
    moves.foldLeft(new TicTacToeBoard())((b, m) => b.makeMove(m))

  test("takes a forced win without transposing the returned coordinates") {
    assert(MCTSMove(4000, 1, 42)(position(1, 4, 2, 5)) == 3)
  }
  test("plays as the actual second player") {
    assert(MCTSMove(4000, 1, 42)(position(1, 4, 2, 5, 9)) == 6)
  }
  test("blocks an immediate loss as second player") {
    assert(MCTSMove(4000, 1, 42)(position(1, 5, 2)) == 3)
  }
  test("search leaves its input unchanged and returns a legal move with one iteration") {
    val b = position(1, 5, 2)
    val before = b.toString
    val result = MCTSMove(1, 1, 2)(b)
    assert(b.getValidMoves.contains(result))
    assert(b.toString == before)
  }
  test("terminal draws have no legal moves and are rejected by search") {
    val b = position(1, 2, 3, 5, 4, 6, 8, 7, 9)
    assert(b.hasWinner.contains(false))
    assert(b.getValidMoves.isEmpty)
    intercept[IllegalArgumentException](MCTSMove()(b))
  }
  test("fixed seed produces the same move and root visits") {
    def search() = {
      val root = new Node(new TicTacToeGameState(), null, null)
      new MonteCarloTreeSearch(root, 100, 2, 72).search()
      root.children.map(n => (n.move.x, n.move.y, n.visits, n.wins))
    }
    assert(search() == search())
  }
  test("both engines account for every simulation without counter races") {
    for (parallel <- List(false, true)) {
      val root = new Node(new TicTacToeGameState(), null, null)
      val mcts = if (parallel) new ParallelMonteCarloTreeSearch(root, 80, 7, 11)
                 else new MonteCarloTreeSearch(root, 80, 7, 11)
      mcts.search()
      assert(root.visits == 560)
      assert(root.children.map(_.visits).sum == 560)
      assert(root.children.forall(n => n.wins >= 0 && n.wins <= n.visits))
    }
  }
  test("nonpositive budgets fail clearly") {
    val root = new Node(new TicTacToeGameState(), null, null)
    intercept[IllegalArgumentException](new MonteCarloTreeSearch(root, 0))
    intercept[IllegalArgumentException](new MonteCarloTreeSearch(root, 1, 0))
  }

  def blankCheckers(player: Player = Players.Player1) = {
    val g = new CheckersGame(player)
    g.board = List.fill(8)(List.fill(8)(Players.None))
    g
  }
  test("crowning one piece cannot crown all pieces or mutate a copied state") {
    val g = blankCheckers()
    g.board = g.updateSpot(6, 0, Players.Player1)
    g.board = g.updateSpot(2, 2, Players.Player1)
    g.board = g.updateSpot(5, 5, Players.Player2)
    val c = g.copy
    c.makeMove(new Move(7, 1, 6, 0))
    assert(c.kings == Set((7, 1)))
    assert(g.kings.isEmpty)
    assert(g.getPiece(6, 0) == Players.Player1)
    assert(Players.Player1.common)
  }
  test("captures are compulsory and a multi-jump preserves the moving player") {
    val g = blankCheckers()
    g.board = g.updateSpot(2, 0, Players.Player1)
    g.board = g.updateSpot(3, 1, Players.Player2)
    g.board = g.updateSpot(5, 3, Players.Player2)
    assert(g.getMoves.forall(m => math.abs(m.x - m.currX) == 2))
    g.makeMove(new Move(4, 2, 2, 0))
    assert(g.currentPlayer == Players.Player1)
    assert(g.getMoves.size == 1)
    g.makeMove(new Move(6, 4, 4, 2))
    assert(g.currentPlayer == Players.Player2)
    assert(g.hasWinner && g.getWinner == Players.Player1)
  }
  test("a blocked checkers player loses rather than wins") {
    val g = blankCheckers()
    g.board = g.updateSpot(7, 1, Players.Player1)
    g.board = g.updateSpot(0, 0, Players.Player2)
    assert(g.hasWinner)
    assert(g.getWinner == Players.Player2)
  }
  test("AB game rejects an occupied landing square") {
    val g = new ABGame(List(Players.Player1, Players.None, Players.Player2, Players.None))
    assert(g.getMoves.forall(m => g.board(m.x) == Players.None))
    intercept[IllegalArgumentException](g.makeMove(new Move(2, 0)))
  }
}
