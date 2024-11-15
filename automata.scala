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
        