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
        
        def adjacent(state: State): List[State] =
            dfa.transitions.filter { case ((fromState, char), toState) =>
                fromState == state && dfa.alphabet.contains(char)
            }
            .values.toList

        
    def main(args: Array[String]): Unit = 
        // Tester des mots
        val states: Set[State] = Set(State("0"), State("1"), State("2"), State("3"), State("4"))
        val alphabet: Set[Char] = Set('1', '2') // '1' pour +1 et '2' pour +2

        val transitions: Transition = Map(
            (State("0"), '1') -> State("1"),
            (State("0"), '2') -> State("2"),
            (State("1"), '1') -> State("2"),
            (State("1"), '2') -> State("3"),
            (State("2"), '1') -> State("3"),
            (State("2"), '2') -> State("4"),
            (State("3"), '1') -> State("4")
        )

        val initialState: State = State("0")
        val acceptedStates: Set[State] = Set(State("4"))

        val automata = Automata(
            states = states,
            transitions = transitions,
            alphabet = alphabet,
            initialState = initialState,
            acceptedStates = acceptedStates
        )

        println(automata.accept(List('1', '1', '1', '1'))) // Devrait imprimer true
        println(automata.accept(List('2', '2')))           // Devrait imprimer true
        println(automata.accept(List('1', '2', '1')))      // Devrait imprimer true
        println(automata.accept(List('1', '1', '2')))      // Devrait imprimer true ou false selon les transitions définies
        println(automata.accept(List('1', '1', '1')))      // Devrait imprimer false
        
