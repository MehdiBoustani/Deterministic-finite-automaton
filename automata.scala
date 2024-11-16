case class State(name: String)

type Word = List[Char]

type Transition = Map[(State, Char), State]

case class Automata(
    states: Set[State],
    transitions: Transition,
    alphabet: Set[Char],
    initialState: State,
    acceptedStates: Set[State]
)

object DFA:
    extension(dfa: Automata)
        def accept(word: Word): Boolean = 
            @annotation.tailrec
            def helper(currentState: State, wordAcc: Word): Boolean = wordAcc match
                case Nil => 
                    dfa.acceptedStates.contains(currentState)
                case head :: tail =>
                    dfa.transitions.get((currentState, head)) match
                        case Some(nextState) =>
                            helper(nextState, tail)
                        case None =>
                            false
                        
            helper(dfa.initialState, word)

// --- Automate des chaînes binaires impaires ---
import DFA.accept
// Def des états
val q0 = State("q0")
val q1 = State("q1")

// Def des transitions
val transitions: Transition = Map(
    (q0, '0') -> q0, // On est pair et on tombe sur un 0, on reste pair
    (q0, '1') -> q1, // On est pair et on tombe sur un 1, on devient impair
    (q1, '0') -> q1, // On est impair et on tombe sur un 0, on reste impair
    (q1, '1') -> q0 // On est impair et on tombe sur un 1, on devient pair
)

// Création de l'automate pour les chaînes binaires impaires
val binaryOddAutomata = Automata(
    states = Set(q0, q1),
    transitions = transitions,
    alphabet = Set('0', '1'),
    initialState = q0,
    acceptedStates = Set(q1)
)

// --- Tests ---
// Test de la fonction accept avec les mots de l'énoncé
println(binaryOddAutomata.accept(List('1', '0', '1', '0', '1', '0', '1', '1'))) // true
println(binaryOddAutomata.accept(List('1', '0', '1', '0', '1', '0', '1', '0'))) // false

// Test avec solve
//println(binaryOddAutomata.solve())