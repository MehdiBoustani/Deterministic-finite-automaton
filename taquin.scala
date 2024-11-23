import scala.util.Random
import ObjectDFA._

object Taquin {
    type Grid = List[Int]

    private def calculateSize(grid: Grid): Int = {
        Math.sqrt(grid.length).toInt
    }

    private def getMoves(grid: Grid): Map[Char, Int] = {
        val size = calculateSize(grid)
        Map(
           
            'u' -> -size,  // Up
            'd' -> size,    // Down
            'l' -> -1,     // Left
            'r' -> 1,      // Right
        )
    }

    case class TaquinState(grid: Grid) extends ObjectDFA.StateDFA {
        lazy val emptyPos: Int = grid.indexOf(0)
        
        // Redéfinition de equals et hashCode pour assurer une comparaison correcte des états
        override def equals(that: Any): Boolean = that match {
            case other: TaquinState => grid == other.grid
            case _ => false
        }
        
        override def hashCode(): Int = grid.hashCode
    }

    class TaquinDFA(val initialState: TaquinState) extends DFA[TaquinState, Char] {
        private val size = calculateSize(initialState.grid)
        private val moves = getMoves(initialState.grid)

        override def states: Set[TaquinState] = {
            def generateAllStates(currentStates: Set[TaquinState]): Set[TaquinState] = {
                val newStates = currentStates.flatMap { state =>
                    moves.keys.flatMap { move =>
                        transition(state, move)
                    }
                }

                if (newStates.subsetOf(currentStates)) currentStates
                else generateAllStates(currentStates ++ newStates)
            }

            generateAllStates(Set(initialState))
        }

        override def alphabet: Set[Char] = moves.keySet

        override def acceptingStates: Set[TaquinState] = {
            val goalGrid = (1 until initialState.grid.length).toList :+ 0
            Set(TaquinState(goalGrid))
        }

        override def transition(state: TaquinState, symbol: Char): Option[TaquinState] = {
            val emptyPos = state.emptyPos
            
            moves.get(symbol).flatMap { offset =>
                val newPos = emptyPos + offset
                
                // On vérifie si le mouvement est valide ET on utilise la nouvelle position
                if (isValidMove(symbol, emptyPos)) {
                    swap(state.grid, emptyPos, newPos).map(TaquinState.apply)
                } else None
            }
        }

        private def swap(grid: Grid, pos1: Int, pos2: Int): Option[Grid] = {
            Some(grid.updated(pos1, grid(pos2)).updated(pos2, grid(pos1)))
        }

        private def isValidMove(symbol: Char, emptyPos: Int): Boolean = {
            val row = emptyPos / size
            val col = emptyPos % size

            symbol match {
                case 'l' => col > 0
                case 'r' => col < size - 1
                case 'u' => row > 0
                case 'd' => row < size - 1
                case _ => false
            }
        }
    }

    def main(args: Array[String]): Unit = {
        val initialState = TaquinState(List(0, 2, 1, 3))
        val dfa = new TaquinDFA(initialState)

        // Pour l'affichage, on calcule la taille
        val size = calculateSize(initialState.grid)
        println(s"État initial : ${initialState.grid.grouped(size).toList.mkString("\n")}")

        val solutions = dfa.solve()
        println("Solutions trouvées :")
        solutions.foreach(solution => println(solution.mkString(" ")))
    }
}