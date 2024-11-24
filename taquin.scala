import scala.util.Random
import ObjectDFA._

object Taquin {
    type Grid = List[Int]

    def calculateSize(grid: Grid): Int = {
        Math.sqrt(grid.length).toInt
    }

    private def getMoves(grid: Grid): Map[Char, Int] = {
        val size = calculateSize(grid)
        Map(
            'r' -> 1,      // Right
            'l' -> -1,     // Left
            'u' -> -size,  // Up
            'd' -> size    // Down
        )
    }

    case class TaquinState(grid: Grid) extends ObjectDFA.StateDFA {
        lazy val emptyPos: Int = grid.indexOf(0)
        
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
            
            // D'abord vérifier si le mouvement est valide
            if (isValidMove(symbol, emptyPos)) {
                // Ensuite calculer la nouvelle position
                moves.get(symbol).flatMap { offset =>
                    val newPos = emptyPos + offset
                    swap(state.grid, emptyPos, newPos).map(TaquinState.apply)
                }
            } else None
        }

        private def swap(grid: Grid, pos1: Int, pos2: Int): Option[Grid] = {
            if (pos1 >= 0 && pos1 < grid.length && pos2 >= 0 && pos2 < grid.length) {
                Some(grid.updated(pos1, grid(pos2)).updated(pos2, grid(pos1)))
            } else None
        }

        private def isValidMove(symbol: Char, emptyPos: Int): Boolean = {
            val row = emptyPos / size
            val col = emptyPos % size

            symbol match {
                case 'r' => col < size - 1  // Peut se déplacer vers la droite
                case 'l' => col > 0         // Peut se déplacer vers la gauche
                case 'u' => row > 0         // Peut se déplacer vers le haut
                case 'd' => row < size - 1  // Peut se déplacer vers le bas
                case _ => false
            }
        }
    }

    def main(args: Array[String]): Unit = {
        val initialState = TaquinState(List(0, 2, 1, 3))
        val dfa = new TaquinDFA(initialState)

        println(s"État initial :")
        val size = calculateSize(initialState.grid)
        println(initialState.grid.grouped(size).map(_.mkString(" ")).mkString("\n"))

        val solutions = dfa.solve() 

        println("Solutions trouvées :")
        if (solutions.isEmpty) {
            println("Aucune solution trouvée.")
        } else {
            solutions.foreach { solution =>
                // Afficher chaque solution brute
                println(solution.mkString(" "))
                
                // Afficher le résultat de l'acceptation de chaque solution
                val moveSequence = solution.mkString("")  // Convertir la solution en une chaîne de mouvements
                println(s"Résultat pour ${moveSequence}: ${dfa.accept(moveSequence)}")
            }
        }

        println(s"Résultat  : ${dfa.accept("rd")}")
    }

}