import ObjectDFA._

/** Deterministic Finite Automaton (DFA) to solve the Taquin problem.
  * It uses a grid (list of  integer here) representation where the puzzle is a permutation of tiles, including an empty space (denoted as 0).
  */
object Taquin {
    type Grid = List[Int]

    /** Calculates the size of the square grid (NxN) based on the total number of elements in the grid.
      *
      * @param grid The grid of the puzzle.
      * @return The size of the grid (number of rows/columns).
      */
    def calculateSize(grid: Grid): Int = {
        Math.sqrt(grid.length).toInt
    }

    /** Retrieves a mapping of possible moves for the empty tile based on its current position.
      * Each move corresponds to a character ('r', 'l', 'u', 'd') and an offset that determines the position change (in the list).
      *
      * @param grid The grid of the puzzle.
      * @return A map of valid moves with their corresponding offsets.
      */
    private def getMoves(grid: Grid): Map[Char, Int] = {
        val size = calculateSize(grid)
        Map(
            'r' -> 1,      // Right
            'l' -> -1,     // Left
            'u' -> -size,  // Up
            'd' -> size    // Down
        )
    }

    /* Represents a state in the Taquin puzzle as a grid configuration. */
    case class TaquinState(grid: Grid) extends StateDFA {
        // Lazy computation of the position of the empty tile (0) in the grid (to avoid to recalculate it).
        lazy val emptyPos: Int = grid.indexOf(0)
    }

    class TaquinDFA(val initialState: TaquinState) extends DFA[TaquinState, Char] {
        private val size = calculateSize(initialState.grid)
        private val moves = getMoves(initialState.grid)

        /** Generates all possible states of the Taquin puzzle starting from the initial state.
          *
          * @return A set of all possible states.
          */
        override def states: Set[TaquinState] = {
            /**
             * Generates all reachable states from an initial puzzle state.
             *
             * @param currentStates A set of states to explore in the current iteration.
             * @param seenStates A set of states already visited to avoid duplicates (used as accumulator).
             * @return The set of all reachable states from the initial state.
             *
            */
            @annotation.tailrec
            def generateAllStates(currentStates: Set[TaquinState], seenStates: Set[TaquinState] = Set.empty): Set[TaquinState] = {
                val newStates = currentStates.flatMap { state =>
                    moves.keys.flatMap { move =>
                        transition(state, move) // Apply transitions to find new states.
                    }
                }

                // If no new states are generated, return the accumulated set of states.
                if (newStates.subsetOf(seenStates)) seenStates
                else generateAllStates(newStates, seenStates ++ newStates)
            }
            generateAllStates(Set(initialState))
        }
        // The alphabet of moves: {'r', 'l', 'u', 'd'}.

        override def alphabet: Set[Char] = moves.keySet

        /** Defines the goal state (accepting state) for the puzzle.
          *
          * @return A set containing the goal state where tiles are ordered, with 0 at the end.
          */
        override def acceptingStates: Set[TaquinState] = {
            val goalGrid = (1 until initialState.grid.length).toList :+ 0
            Set(TaquinState(goalGrid))
        }

        /** Defines the transition function for the DFA, which determines the next state based on the current state and a move.
          *
          * @param state The current state.
          * @param symbol The move to apply ('r', 'l', 'u', 'd').
          * @return The next state after applying the move (in a Some), if valid (None if not).
          */
        override def transition(state: TaquinState, symbol: Char): Option[TaquinState] = {
            val emptyPos = state.emptyPos
            
            // Check if the move is valid.
            if (isValidMove(symbol, emptyPos)) {
                // Compute the new position of the empty tile after the move.
                moves.get(symbol).flatMap { offset =>
                    val newPos = emptyPos + offset

                    // Swap the empty tile with the tile at the new position to create a new state.
                    swap(state.grid, emptyPos, newPos).map(TaquinState.apply)
                }
            } else None
        }
        /** Swaps two positions in the grid to simulate a move.
          *
          * @param grid The current grid.
          * @param pos1 The position of the empty tile.
          * @param pos2 The position to swap with.
          * @return The new grid after swapping (in a Some), if the positions are valid (None if not).
          */
        private def swap(grid: Grid, pos1: Int, pos2: Int): Option[Grid] = {
            if (pos1 >= 0 && pos1 < grid.length && pos2 >= 0 && pos2 < grid.length) {
                Some(grid.updated(pos1, grid(pos2)).updated(pos2, grid(pos1)))
            } else None
        }
        /** Checks if a move is valid based on the current position of the empty tile.
          *
          * @param symbol The move to validate.
          * @param emptyPos The current position of the empty tile.
          * @return True if the move is valid, false otherwise.
          */
        private def isValidMove(symbol: Char, emptyPos: Int): Boolean = {
            val row = emptyPos / size
            val col = emptyPos % size

            symbol match {
                case 'r' => col < size - 1  // Can move right if not in the last column.
                case 'l' => col > 0         // Can move left if not in the first column.
                case 'u' => row > 0         // Can move up if not in the first row.
                case 'd' => row < size - 1  // Can move down if not in the last row.
                case _ => false
            }
        }

        /** Heuristic function h1: Counts the number of misplaced tiles.
         *
         * @param state The current state of the puzzle.
         * @return The number of tiles that are not in their goal position.
         */
        def h1(state: TaquinState): Double = {
            val goalGrid = acceptingStates.head.grid
            state.grid.zip(goalGrid).count { case (currentTile, goalTile) =>
                currentTile != goalTile
            }.toDouble
        }

        def h2(state: TaquinState): Double = {
            val goalGrid = acceptingStates.head.grid

            state.grid.zipWithIndex.foldLeft(0.0) { case (accumDist, (tile, currentIndex)) =>
                // Calculate the row and column of the tile in the current and goal grid
                val goalIndex = goalGrid.indexOf(tile)
                val currentRow = currentIndex / size
                val currentCol = currentIndex % size
                val goalRow = goalIndex / size
                val goalCol = goalIndex % size

                // Calculate the Manhattan distance for the tile
                val dist = Math.abs(currentRow - goalRow) + Math.abs(currentCol - goalCol)
                accumDist + dist
            }
        }

    }
    /**
      * Main entry point of the application.
      *
      * @param args Command-line arguments (not used in this implementation).
      * The program initializes a puzzle state and computes all reachable states.
      */
    def main(args: Array[String]): Unit = {
        // Define the initial state of the puzzle.
        // val initialState = TaquinState(List(2, 3, 6, 1, 0, 5, 7, 8, 4))
        // val initialState = TaquinState(List(1, 2, 3, 4, 5, 6, 7, 0, 8))
        val initialState = TaquinState(List(0, 2, 1, 3))
        val dfa = new TaquinDFA(initialState)

        println(s"État initial :")
        val size = calculateSize(initialState.grid)
        println(initialState.grid.grouped(size).map(_.mkString(" ")).mkString("\n"))

        // Solve the puzzle by finding all possible solutions.
        val solutions = dfa.solve() 

        println("Solutions trouvées :")
        if (solutions.isEmpty) {
            println("Aucune solution trouvée.")
        } else {
            solutions.foreach { solution =>
                // Print each solution as a sequence of moves.
                println(solution.mkString(" "))
                
                // Verify if the solution leads to the goal state.
                val moveSequence = solution.mkString("")  // Convertir la solution en une chaîne de mouvements
                println(s"Résultat pour ${moveSequence}: ${dfa.accept(moveSequence)}")
            }
        }
    }
}