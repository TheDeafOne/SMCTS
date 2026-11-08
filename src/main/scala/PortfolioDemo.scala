import algorithms.*
import games.*
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Random

/** Reproduces the README figures from actual searches and complete games. */
object PortfolioDemo {
  val iterations = 2000
  val gamesPerOpponent = 100
  val seed = 42
  private val solved = mutable.Map.empty[(Int, Int, Boolean), Int]

  private def value(b: TicTacToeBoard): Int = solved.getOrElseUpdate(
    (b.player1Pieces, b.player2Pieces, b.getNextPlayer), {
      if (b.hasWinner.isDefined) {
        if (!b.hasWinner.get) 0 else if (b.getWinner == 1) 1 else -1
      } else {
        val scores = b.getValidMoves.map(m => value(b.makeMove(m)))
        if (b.getNextPlayer) scores.max else scores.min
      }
    })

  private def perfectMove(b: TicTacToeBoard): Int =
    b.getValidMoves.maxBy(m => value(b.makeMove(m)) * (if (b.getNextPlayer) 1 else -1))

  private def play(opponent: String, game: Int): Int = {
    val mctsFirst = game % 2 == 0
    val choose = MCTSMove(iterations, 1, seed + game)
    val random = new Random(seed + 10000 + game)
    var board = new TicTacToeBoard()
    while (board.hasWinner.isEmpty) {
      val mctsTurn = board.getNextPlayer == mctsFirst
      val move = if (mctsTurn) choose(board)
                 else if (opponent == "minimax") perfectMove(board)
                 else board.getValidMoves(random.nextInt(board.getValidMoves.size))
      require(board.getValidMoves.contains(move), "benchmark produced an illegal move")
      board = board.makeMove(move)
    }
    if (!board.hasWinner.get) 0
    else if (board.getWinner == (if (mctsFirst) 1 else 2)) 1 else -1
  }

  private def svg(width: Int, height: Int, body: String): String =
    s"""<svg xmlns="http://www.w3.org/2000/svg" width="$width" height="$height" viewBox="0 0 $width $height">
      <rect width="100%" height="100%" rx="18" fill="#111827"/>
      <style>text{font-family:system-ui,sans-serif;fill:#e5e7eb}.muted{fill:#a6b1c3}.label{font-size:15px}.title{font-size:25px;font-weight:650}</style>
      $body</svg>"""

  def main(args: Array[String]): Unit = {
    Files.createDirectories(Path.of("docs"))
    val rows = List("random", "minimax").map { opponent =>
      val outcomes = (0 until gamesPerOpponent).map(game => play(opponent, game))
      val row = (opponent, outcomes.count(_ == 1), outcomes.count(_ == 0), outcomes.count(_ == -1))
      println(s"${row._1}: wins=${row._2} draws=${row._3} losses=${row._4}")
      row
    }
    Files.writeString(Path.of("docs/results.csv"),
      "opponent,games,mcts_wins,draws,mcts_losses,iterations,rollouts,seed,seat_protocol\n" +
      rows.map((o, w, d, l) => s"$o,$gamesPerOpponent,$w,$d,$l,$iterations,1,$seed,alternating").mkString("\n") + "\n")
    val bars = rows.zipWithIndex.map { case ((opponent, wins, draws, losses), index) =>
      val y = 106 + index * 100
      val counts = List(wins, draws, losses)
      val colors = List("#34d399", "#60a5fa", "#fb7185")
      val segments = counts.indices.map { i =>
        val x = 175 + counts.take(i).sum * 5
        val w = counts(i) * 5
        s"""<rect x="$x" y="$y" width="$w" height="35" fill="${colors(i)}"/>"""
      }.mkString
      s"""<text x="30" y="${y + 23}" class="label">vs $opponent</text>$segments
        <text x="175" y="${y + 58}" class="label">$wins wins · $draws draws · $losses losses</text>"""
    }.mkString
    Files.writeString(Path.of("docs/benchmark.svg"), svg(740, 360,
      s"""<text x="30" y="43" class="title">How does search change the game?</text>
        <text x="30" y="72" class="label muted">100 games per opponent · 50 in each seat · 2,000 iterations per move</text>
        $bars<text x="30" y="330" class="label muted">Green: MCTS win    Blue: draw    Pink: MCTS loss · seed 42 + game index</text>"""))

    val board = List(1, 5, 2).foldLeft(new TicTacToeBoard())((b, m) => b.makeMove(m))
    val root = new Node(new TicTacToeGameState(Players.Player2, board), null, null)
    val (_, chosen) = new MonteCarloTreeSearch(root, iterations, 1, seed).search()
    val chosenSquare = (chosen.y - 1) * 3 + chosen.x
    val stats = root.children.sortBy(n => (n.move.y - 1) * 3 + n.move.x)
    Files.writeString(Path.of("docs/search.csv"), "square,visits,value_for_O,selected\n" +
      stats.map(n => s"${(n.move.y - 1) * 3 + n.move.x},${n.visits},${n.valueFor(Players.Player2)},${n == root.children.find(_.move == chosen).get}").mkString("\n") + "\n")
    val cells = (1 to 9).map { square =>
      val x = 30 + ((square - 1) % 3) * 100
      val y = 100 + ((square - 1) / 3) * 100
      val piece = if ((board.player1Pieces & (1 << (square - 1))) != 0) "X"
                  else if ((board.player2Pieces & (1 << (square - 1))) != 0) "O" else square.toString
      val fill = if (square == chosenSquare) "#065f46" else "#1f2937"
      s"""<rect x="$x" y="$y" width="92" height="92" rx="8" fill="$fill"/>
        <text x="${x + 46}" y="${y + 60}" text-anchor="middle" font-size="38">$piece</text>"""
    }.mkString
    val visits = stats.zipWithIndex.map { case (n, index) =>
      val square = (n.move.y - 1) * 3 + n.move.x
      val y = 107 + index * 49
      val width = 290.0 * n.visits / stats.map(_.visits).max
      s"""<text x="355" y="${y + 19}" class="label">$square</text>
        <rect x="380" y="$y" width="$width" height="25" rx="4" fill="${if (square == chosenSquare) "#34d399" else "#60a5fa"}"/>
        <text x="${390 + width}" y="${y + 19}" class="label">${n.visits}</text>"""
    }.mkString
    Files.writeString(Path.of("docs/search.svg"), svg(800, 460,
      s"""<text x="30" y="43" class="title">O needs to block square 3</text>
        <text x="30" y="72" class="label muted">An actual UCT search: $iterations iterations, one simulation each, seed $seed</text>
        $cells$visits<text x="30" y="434" class="label muted">Green: selected move · bars: visits to each possible next move</text>"""))
    println(s"Tactical figure selected square $chosenSquare; figures and raw CSV written to docs/")
  }
}
