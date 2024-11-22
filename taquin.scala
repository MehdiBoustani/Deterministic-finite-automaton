object Taquin2x2 {

    // Définitions des mouvements possibles
    trait Move
    case object Left extends Move
    case object Right extends Move
    case object Up extends Move
    case object Down extends Move

    case class TaquinState(grid: Vector[Vector[Int]], emptyPos: (Int, Int)) extends ObjectDFA.StateDFA

    class TaquinDFA(val initialState: TaquinState) extends DFA[TaquinState, Move] {
        // Générer tous les états possibles pour un Taquin 2x2
        override def states: Set[TaquinState] = {
            // j'y arrive pas :(
        }

        // Alphabet
        override def alphabet: Set[Move] = Set(Left, Right, Up, Down)

        // États acceptants : quand le puzzle est résolu
        override def acceptingStates: Set[TaquinState] = Set(
            TaquinState(Vector(Vector(1, 2), Vector(3, 0)), (1, 1)) // état final
        )

        override def transition(state: TaquinState, symbol: Move): Option[TaquinState] = {
            val (x, y) = state.emptyPos
            val grid = state.grid

            // Fonction pour effectuer un mouvement et retourner le nouvel état
            def moveIfValid(newX: Int, newY: Int): Option[TaquinState] = {
                if (newX >= 0 && newX < 2 && newY >= 0 && newY < 2) {
                    swap(grid, (x, y), (newX, newY)) match {
                    case Some(newGrid) => Some(TaquinState(newGrid, (newX, newY))) // Si l'échange a réussi, retourner le nouvel état
                    case None => None // Si l'échange échoue, retourner None
                    }
                } else {
                        None
                }
            }

            symbol match {
                case Left  => moveIfValid(x, y - 1) // Déplacer vers la gauche
                case Right => moveIfValid(x, y + 1) // Déplacer vers la droite
                case Up    => moveIfValid(x - 1, y) // Déplacer vers le haut
                case Down  => moveIfValid(x + 1, y) // Déplacer vers le bas
                case _     => None // Mouvement impossible
            }
        }


        private def swap(grid: Vector[Vector[Int]], pos1: (Int, Int), pos2: (Int, Int)): Option[Vector[Vector[Int]]] = {
            val (x1, y1) = pos1
            val (x2, y2) = pos2
            
            if (x1 < 0 || x1 >= grid.size || y1 < 0 || y1 >= grid(0).size ||
                x2 < 0 || x2 >= grid.size || y2 < 0 || y2 >= grid(0).size) {
                None
            } else {
                // Récupérer les lignes
                val row1 = grid(x1)
                val row2 = grid(x2)

                // Échanger les éléments dans les lignes concernées
                val newRow1 = row1.updated(y1, row2(y2))
                val newRow2 = row2.updated(y2, row1(y1))

                // Reconstruire la grille avec les lignes mises à jour
                Some(grid.updated(x1, newRow1).updated(x2, newRow2))
            }
        }
    }
}
//Tests
import Taquin2x2._
