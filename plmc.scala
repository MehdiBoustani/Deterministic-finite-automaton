object ObjectDFA:

    type Word[+A] = Seq[A]

    trait DFA[Q, A]: // Q = state type, A = Symbol type
        def states: Set[Q]
        def alphabet: Set[A] 
        def initialState: Q
        def acceptingStates: Set[Q] // SUBSET OF states !!!!

        def transition(state: Q, symbol: A): Option[Q]

        def getAdjacentStates(state: Q): Set[(A, Q)] = 
            alphabet.flatMap(symbol => transition(state, symbol).map(nextState => (symbol, nextState)))

        private def isAccepted(state: Q): Boolean = 
            acceptingStates.contains(state)

        def accept(word: Word[A]): Boolean = 
            // dfa.acceptingStates.contains(word.foldLeft(dfa.initialState)((state, symbol) => dfa.transition(state, symbol)))
            val finalState = word.foldLeft(Option(initialState)) ( (currentStateOpt, symbol) =>
                currentStateOpt.flatMap(state => transition(state, symbol)))
            finalState match 
                case None => false
                case Some(state) => isAccepted(state)

        // def solve(): List[Word[A]] = 
        //     @annotation.tailrec 
        //     def solveHelper(word: Word[A], visited: Set[Q], adjacent: Set[(A, Q)], solution: List[Word[A]]): List[Word[A]] = 
        //         if (adjacent.isEmpty)  // no more transition
        //             if (accept(word))  word.reverse :: solution // a solution path is found 
        //             else solution 
        //         else 
        //             val symbol = adjacent.head._1
        //             val newState = adjacent.head._2
        //             if (visited.contains(newState)) 
        //                 solveHelper(word, visited, adjacent.tail, solution)
        //             else 
        //                 val newWord = symbol +: word

        //                 solveHelper(newWord, visited + newState, getAdjacentStates(newState) , solution)
                        
        //     solveHelper(Nil, Set(initialState), getAdjacentStates(initialState), Nil)

        def solve(): List[Word[A]] = {
            @annotation.tailrec
            def solveHelper(
                stack: List[(Word[A], Set[Q], Set[(A, Q)])], 
                solution: List[Word[A]]
            ): List[Word[A]] = stack match {
                case Nil => solution // All paths explored, return accumulated solutions
                case (word, visited, adjacent) :: rest =>
                    if (adjacent.isEmpty) { // No more transitions
                        if (accept(word)) solveHelper(rest, word.reverse :: solution) // Found a solution
                        else solveHelper(rest, solution)
                    } else {
                        val (symbol, newState) = adjacent.head
                        val remainingAdjacent = adjacent.tail

                        if (visited.contains(newState)) {
                            // Skip already visited states
                            solveHelper((word, visited, remainingAdjacent) :: rest, solution)
                        } else {
                            val newWord = symbol +: word
                            val newVisited = visited + newState
                            val newAdjacent = getAdjacentStates(newState)

                            // Add new state exploration to stack
                            solveHelper(
                                (word, visited, remainingAdjacent) :: 
                                (newWord, newVisited, newAdjacent) :: 
                                rest, 
                                solution
                            )
                        }
                    }
            }

            // Start with the initial state
            solveHelper(List((Nil, Set(initialState), getAdjacentStates(initialState))), Nil)
        }

object PLMC:
    enum Item:
        case Farmer, Goat, Wolf, Gabbage
        def toChar: Char = this match
            case Farmer  => 'P'
            case Wolf    => 'L'
            case Goat    => 'M'
            case Gabbage => 'C'

    val P = Item.Farmer
    val L = Item.Wolf
    val M = Item.Goat
    val C = Item.Gabbage

    case class State[T](left: Set[T], right: Set[T])
    // case class State(left: Set[Item], right: Set[Item]) extends ObjectDFA.StateDFA[Item]

    val s0 = State(Set(P, L, M, C), Set())
    val s1 = State(Set(L, M, C), Set(P))  // DANGER

    val s2 = State(Set(M, C), Set(P, L))   // DANGER
    val s3 = State(Set(L, C), Set(P, M))
    val s4 = State(Set(L, M), Set(P, C))  // DANGER

    val s5 = State(Set(P, M, C), Set(L)) 
    val s6 = State(Set(P, L, C), Set(M)) 
    val s7 = State(Set(P, L, M), Set(C))

    val s8 = State(Set(L), Set(P, M, C))
    val s9 = State(Set(M), Set(P, L, C))
    val s10 = State(Set(C), Set(P, L, M))

    val s11 = State(Set(P, L), Set(M, C))  // DANGER
    val s12 = State(Set(P, M), Set(L, C))
    val s13 = State(Set(P, C), Set(L, M))  // DANGER

    val s14 = State(Set(P), Set(L, M, C)) // DANGER
    val s15 = State(Set(), Set(P, L, M, C)) // Accepter 

    private def isSink(state: State[Item]): Boolean = 
        
        val wolfGoat = ((state.left.contains(M) && state.left.contains(L) && !state.left.contains(P)) || 
                        (state.right.contains(M) && state.right.contains(L) && !state.right.contains(P)))

        val goatGabbage = ((state.left.contains(M) && state.left.contains(C) && !state.left.contains(P)) || 
                        (state.right.contains(M) && state.right.contains(C) && !state.right.contains(P)))
                             
        (wolfGoat || goatGabbage) // true if sink

    private def isValid(state: State[Item], symbol: Char): Boolean = symbol match 
        case 'p' => true
        case 'l' => ( (state.left.contains(L) && state.left.contains(P)) || (state.right.contains(L) && state.right.contains(P)) )
        case 'm' => ( (state.left.contains(M) && state.left.contains(P)) || (state.right.contains(M) && state.right.contains(P)) )
        case 'c' => ( (state.left.contains(C) && state.left.contains(P)) || (state.right.contains(C) && state.right.contains(P)) )
        case _ => false

    private def moveNext(state: State[Item], symbol: Char): State[Item] = symbol match
        case 'p' => if state.left.contains(P) then State(state.left - P, state.right + P) else State(state.left + P, state.right - P)
        case 'l' => if (state.left.contains(P) && state.left.contains(L)) then State(state.left - P - L, state.right + P + L) else State(state.left + P + L, state.right - P - L)
        case 'm' => if (state.left.contains(P) && state.left.contains(M)) then State(state.left - P - M, state.right + P + M) else State(state.left + P + M, state.right - P - M)
        case 'c' => if (state.left.contains(P) && state.left.contains(C)) then State(state.left - P - C, state.right + P + C) else State(state.left + P + C, state.right - P - C)

    case class plmcDFA() extends ObjectDFA.DFA[State[Item], Char]:
        override def states = Set(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)
        override def alphabet = Set('p', 'l', 'm', 'c')
        override def initialState = s0
        override def acceptingStates = Set(s15)

        override def transition(state: State[Item], symbol: Char): Option[State[Item]] = isValid(state, symbol) match
            case false => None
            case true => 
                val nextState = moveNext(state, symbol) 
                // println(nextState)
                if !isSink(nextState) then Some(nextState) else None

object MyApp {
  def main(args: Array[String]): Unit = {
    val plmc = PLMC.plmcDFA()
    // println(plmc.getAdjacentStates(plmc.initialState))
    println(plmc.solve())
  }
}