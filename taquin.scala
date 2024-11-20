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

        // Fonction de transition
        override def transition(state: TaquinState, symbol: Move): Option[TaquinState] = {
            val (x, y) = state.emptyPos
            val grid = state.grid

            symbol match {
                case Left if y > 0 => // Déplacer vers la gauche
                    val newGrid = swap(grid, (x, y), (x, y - 1))
                    Some(TaquinState(newGrid, (x, y - 1)))
                case Right if y < 1 => // Déplacer vers la droite
                    val newGrid = swap(grid, (x, y), (x, y + 1))
                    Some(TaquinState(newGrid, (x, y + 1)))
                case Up if x > 0 => // Déplacer vers le haut
                    val newGrid = swap(grid, (x, y), (x - 1, y))
                    Some(TaquinState(newGrid, (x - 1, y)))
                case Down if x < 1 => // Déplacer vers le bas
                    val newGrid = swap(grid, (x, y), (x + 1, y))
                    Some(TaquinState(newGrid, (x + 1, y)))
                case _ => None // Mouvement impossible
            }
        }

        // Fonction pour échanger deux cases de la grille
        private def swap(grid: Vector[Vector[Int]], pos1: (Int, Int), pos2: (Int, Int)): Vector[Vector[Int]] = {
            val (x1, y1) = pos1
            val (x2, y2) = pos2
            grid.updated(x1, grid(x1).updated(y1, grid(x2)(y2)))
                .updated(x2, grid(x2).updated(y2, grid(x1)(y1)))
        }
    }
}
//Tests
import Taquin2x2._
