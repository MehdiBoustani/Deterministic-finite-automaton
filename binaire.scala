// --- Automate des chaînes binaires impaires ---
import ObjectDFA._

object BinaryOddDFA:

    // Définir les états comme des sous-types de `StateDFA`
    case class BinaryState(name: String) extends StateDFA

    // Définir les états
    val even = BinaryState("even") // État où le nombre de `1` est pair
    val odd = BinaryState("odd") // État où le nombre de `1` est impair

    // Définir les transitions pour les chaînes binaires impaires
    case class BinaryOddDFA() extends DFA[BinaryState, Char]:
        override def states: Set[BinaryState] = Set(even, odd)
        override def alphabet: Set[Symbol[Char]] = Set('0', '1')
        override def initialState: BinaryState = even
        override def acceptingStates: Set[BinaryState] = Set(odd)

        // Fonction de transition
        override def transition(state: BinaryState, symbol: Char): Option[BinaryState] = (state, symbol) match
            case (`even`, '0') => Some(even)
            case (`even`, '1') => Some(odd)
            case (`odd`, '0') => Some(odd)
            case (`odd`, '1') => Some(even)
            case _           => None
        
    def main(args: Array[String]): Unit = {
        val dfa = new BinaryOddDFA()

        println(s"État initial :")

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

        println(s"Résultat  : ${dfa.accept("01011")}")
    }