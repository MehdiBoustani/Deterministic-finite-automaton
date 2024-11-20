// --- Automate des chaînes binaires impaires ---
import ObjectDFA._

object BinaryOddDFA:

    // Définir les états comme des sous-types de `StateDFA`
    case class BinaryState(name: String) extends StateDFA

    // Définir les états
    val q0 = BinaryState("q0") // État où le nombre de `1` est pair
    val q1 = BinaryState("q1") // État où le nombre de `1` est impair

    // Définir les transitions pour les chaînes binaires impaires
    case class BinaryOddDFA() extends DFA[BinaryState, Char]:
        override def states: Set[BinaryState] = Set(q0, q1)
        override def alphabet: Set[Symbol[Char]] = Set('0', '1')
        override def initialState: BinaryState = q0
        override def acceptingStates: Set[BinaryState] = Set(q1)

        // Fonction de transition
        override def transition(state: BinaryState, symbol: Char): Option[BinaryState] = (state, symbol) match
            case (`q0`, '0') => Some(q0)
            case (`q0`, '1') => Some(q1)
            case (`q1`, '0') => Some(q1)
            case (`q1`, '1') => Some(q0)
            case _           => None


val binaryOddDFA = BinaryOddDFA.BinaryOddDFA()
val seqWord = "110"
println(s"Accepté ? ${binaryOddDFA.accept(seqWord)}")

// Test avec solve
println("Solutions générées par solve :")
println(binaryOddDFA.solve())

        // println(binaryOddDFA.accept(Seq('1', '0', '1', '0', '1', '0', '1', '1'))) // true
        // println(binaryOddDFA.accept(Seq('1', '0', '1', '0', '1', '0', '1', '0'))) // false



