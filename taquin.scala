import scala.util.Random
import ObjectDFA._


object Taquin {
    // Redéfinition de Grid comme une liste simple d'entiers
    type Grid = List[Int]

    // Définition des mouvements dans un Map associant chaque direction à un déplacement en index
    val moves: Map[Char, Int] = Map(
        'l' -> -1,  // Left : déplace vers la gauche
        'r' -> 1,   // Right : déplace vers la droite
        'u' -> -2,  // Up : déplace vers le haut (grille 2x2)
        'd' -> 2    // Down : déplace vers le bas (grille 2x2)
    )

    case class TaquinState(grid: Grid) extends ObjectDFA.StateDFA {
        lazy val emptyPos: Int = grid.indexOf(0)
    }

    class TaquinDFA(val initialState: TaquinState) extends DFA[TaquinState, Char] {

        // Générer tous les états possibles pour un Taquin 2x2
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

        // Alphabet des mouvements (basé sur les clés du Map)
        override def alphabet: Set[Char] = moves.keySet

        // États acceptants : quand le puzzle est résolu
        override def acceptingStates: Set[TaquinState] = Set(
            TaquinState(List(1, 2, 3, 0))
        )

        // Transition entre les états
        override def transition(state: TaquinState, symbol: Char): Option[TaquinState] = {
            val emptyPos = state.emptyPos
            val grid = state.grid

            // Vérifier si le symbole est valide et récupérer l'offset correspondant
            moves.get(symbol).flatMap { offset =>
                val newPos = emptyPos + offset
                
                // Vérifier si le mouvement est valide (ne traverse pas les bords)
                if (isValidMove(emptyPos, newPos)) {
                    swap(grid, emptyPos, newPos).map(TaquinState.apply)
                } else None
            }
        }

        // Fonction pour échanger les éléments dans la grille
        private def swap(grid: Grid, pos1: Int, pos2: Int): Option[Grid] = {
            if (pos1 >= 0 && pos1 < grid.length && pos2 >= 0 && pos2 < grid.length) {
                val newGrid = grid.updated(pos1, grid(pos2)).updated(pos2, grid(pos1))
                Some(newGrid)
            } else None
        }

        // Vérifie si un mouvement est valide (ne traverse pas les bords de la grille)
        private def isValidMove(currentPos: Int, newPos: Int): Boolean = {
            if (newPos < 0 || newPos >= 4) return false  // Hors limites
            
            val currentRow = currentPos / 2
            val currentCol = currentPos % 2
            val newRow = newPos / 2
            val newCol = newPos % 2
            
            // Vérifie que le mouvement ne traverse pas les bords
            (currentRow - newRow).abs + (currentCol - newCol).abs == 1
        }
    }

    // Exemple d'utilisation
    def main(args: Array[String]): Unit = {
        val initialState = TaquinState(List(0, 2, 1, 3))
        val dfa = new TaquinDFA(initialState)

        // Affichage de l'état initial
        println(s"État initial : ${initialState.grid}")

        // Appel de la méthode solve pour obtenir les chemins vers un état accepteur
        val solutions = dfa.solve()

        // Affichage des solutions trouvées
        println("Solutions trouvées :")
        solutions.foreach(solution => println(solution.mkString(" ")))

        println(s"Félicitations ! Vous avez résolu le puzzle : ${initialState.grid}")
    }
}
