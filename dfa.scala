object ObjectDFA:

    trait StateDFA // Defined interface for a DFA state
    type Symbol[+A] = A // symbol of an alphabet 
    type Word[+A] = Seq[Symbol[A]] // sequence of symbols 

    /* Defining an interface for a Deterministic Finite Aotomaton using a tuple (Q, Σ, δ, s, F) where: 
     *
     * Q is a finite set of states
     * Σ is a finite alphabet
     * δ: Q x Σ -> Q a transition function allowing choose the next state
     * s is the initial state
     * F ⊆ Q a set of accepting states 
     */
    trait DFA[S <: StateDFA, A]: // S = state subtype of StateDFA, A = Type of the alphabet symbols
        def states: Set[S] //  Q
        def alphabet: Set[Symbol[A]] //  Σ
        def transition(state: S, symbol: Symbol[A]): Option[S] //  δ
        def initialState: S //  s
        def acceptingStates: Set[S] //  F (Assuming that it is a subset of states)
        
    extension[S <: StateDFA, A](dfa: DFA[S, A]) 

        /** This getAdjacentStates method returns a set of allowed adjacent states from a given state 
         * getAdjacentStates(q) = {(σ, q') ∈ Σ × Q|q' = δ(q, σ)}
         * 
         * @param state a state 
         *    
         * @return  A set of tuples where each element contains the symbol and 
         *          the transition (new state) using the symbol      
         */ 
        def getAdjacentStates(state: S): Set[(Symbol[A], S)] = 
            dfa.alphabet.flatMap(symbol => dfa.transition(state, symbol).map(nextState => (symbol, nextState)))

        /** This isAccepted method determines whether or not a given state is accepted 
         * 
         * @param state a state
         *   
         * @return  true if the state is accepted, 
         *          false otherwise
         */
        private def isAccepted(state: S): Boolean = 
            dfa.acceptingStates.contains(state)

        /** This accept method determines whether or not a given word leads the DFA to an accepting state (starting at initial state s)
         *
         * @param word: a word 
         * 
         * @return true if the word leads to an accepting state
         *        false otherwise
         */
        def accept(word: Word[A]): Boolean = 
            val finalState = word.foldLeft(Option(dfa.initialState)) ( (currentStateOpt, symbol) =>
                currentStateOpt.flatMap(state => dfa.transition(state, symbol)))
            finalState match 
                case None => false
                case Some(state) => isAccepted(state)
            
        /** This solve method gets all possible words leading to an acyclic solution path for the DFA starting at initial state
         * Returns a list of words leading initial state to an accepting state
         * 
         * @return List[Word[A]]: a list of words leading to a solution
         */
        def solve(): List[Word[A]] = 
            /** solveHelper function gets all possible words leading to solutions (acyclic paths) for a DFA starting at a given state
             * Returns a list of words leading a given state to an accepting state 
             * 
             * @param paths A list of paths, where each path is represented as a tuple:
             *              - S: The current state in the DFA.
             *              - Word[A]: The word leading to the current state.
             *              - Set[S]: The set of states visited so far
             *              - Set[(Symbol[A], S)]: The set of adjacent transitions from the current state.
             * @param solution current solution 
             * 
             * @return List[Word[A]]: a list of words leading to a solution
             */
            @annotation.tailrec
            def solveHelper(paths: List[(S, Word[A], Set[S], Set[(Symbol[A], S)])], solution: List[Word[A]]): List[Word[A]] = paths match 
                case Nil => solution // All paths explored, return solutions
                case (currentState, word, visited, adjacent) :: rest => 
                    
                    // We can check if the word is accepted : efficiency: we have to recompute transitions, which is not that efficient
                    // reversedWord = word.reverse
                    // if (accept(reversedWord)) solveHelper(rest, reversedWord :: solution) // Add the accepted word to solution

                    // Check if the current state is an accepting state : We are using this one for more efficiency ! 
                    if (isAccepted(currentState)) solveHelper(rest, word.reverse :: solution) // Add the word leading to this accepting state
                    else 
                        // remove visited states
                        val nonVisitedAdjacents = adjacent.filterNot((_, adjState) => visited.contains(adjState)) 

                        if (nonVisitedAdjacents.isEmpty) solveHelper(rest, solution) // no more transitions -> explore other paths
                        
                        else 
                            // Explore the next transition
                            val (symbol, nextState) = nonVisitedAdjacents.head // take a valid adjacent for a new state
                            val remainingAdjacent = nonVisitedAdjacents.tail

                            // Add new path to explore
                            solveHelper(
                                (nextState, symbol +: word, visited + nextState, getAdjacentStates(nextState)) :: // new state path
                                (currentState, word, visited, remainingAdjacent) :: // last state path 
                                rest, // other paths
                                solution
                            )
                            
            // Start with the initial state
            solveHelper(List((dfa.initialState, Nil, Set(dfa.initialState), getAdjacentStates(dfa.initialState))), Nil)

        /** This lazySolve method computes "on-demand" solution paths (acyclic paths leading to accepting states) 
         * for the DFA starting from the initial state. It generates a lazy list of words corresponding to these paths.
         *
         * @return LazyList[Word[A]]: A lazy list of words, each representing a valid sequence of symbols that leads 
         *                            the DFA from the initial state to an accepting state.
         */
        def lazySolve(): LazyList[Word[A]] = 
             /** The lazyHelper function generates "on-demand" words leading to accepting states for the DFA.
             *
             * @param paths A LazyList of paths, where each path is represented as a tuple:
             *              - (S): The current state in the DFA.
             *              - (Word[A]): The word leading to the current state.
             *              - (Set[S]): The set of states visited so far
             *              - (Set[(Symbol[A], S)]): The set of adjacent transitions from the current state.
             * 
             * @return Option[(Word[A], List[(S, Word[A], Set[S], Set[(Symbol[A], S)])])]: An optional tuple containing:
             *         - The word leading to an accepting state (if found).
             *         - The updated list of remaining paths to explore.
             *         Returns `None` if no accepting state is found.
             */
            @annotation.tailrec
            def lazyHelper(paths: LazyList[(S, Word[A], Set[S], Set[(Symbol[A], S)])]): Option[(Word[A], LazyList[(S, Word[A], Set[S], Set[(Symbol[A], S)])])] = paths match
                case LazyList() => None
                case (currentState, word, visited, adjacent) #:: rest => 
                    if (isAccepted(currentState)) Some((word.reverse, rest)) // goal reached
                    else 
                        // exclude visited states
                        val nonVisitedAdjacents = adjacent.filterNot((_, adjState) => visited.contains(adjState))

                        if (nonVisitedAdjacents.isEmpty) lazyHelper(rest) // no adjacent states (i.e no more transitions) -> explore other paths
                        else 
                            val (symbol, nextState) = nonVisitedAdjacents.head // take a valid adjacent for a new state
                            val remainingAdjacents = nonVisitedAdjacents.tail 

                            lazyHelper(
                                       (nextState, symbol +: word, visited + nextState, getAdjacentStates(nextState)) #:: // new state path 
                                       (currentState, word, visited, remainingAdjacents) #::  // last state path -> explore other adjacent states
                                        rest // other paths
                                      )

            LazyList.unfold(LazyList((dfa.initialState, Seq.empty[Symbol[A]], Set(dfa.initialState), getAdjacentStates(dfa.initialState)))) { paths => lazyHelper(paths)}
        
        /** this heuristicSolve method computes solution paths "on demand" for the DFA starting from the initial state
         *   using a heuristic-driven approach, generating a lazy list of words leading to accepting states,
         *       prioritizing paths with lower heuristic costs
         * 
         * @param heuristic A function (S => Double) that computes the cost of a given state
         * 
         * @return LazyList[Word[A]]: A lazy list of words, leading to an accepting state
         *                             guided by the heuristic.
         */
        def heuristicSolve(heuristic: S => Double): LazyList[Word[A]] = 
            /** This heuristicHelper function gets the word (if exists) with lowest heuristic cost
             *   leading to an accepting state without cycles
             * 
             * @param paths A LazyList of paths to explore, where each path is represented as a tuple:
             *              - S: The current state in the DFA.
             *              - Word[A]: The sequence of symbols leading to the current state.
             *              - Set[S]: The set of states visited so far, to prevent revisiting and cycles.
             *              - Set[(Symbol[A], S)]: The set of adjacent transitions from the current state.
             * 
             * @return Option[(Word[A], List[(S, Word[A], Set[S], Set[(Symbol[A], S)])])]: An optional tuple containing:
             *         - The word leading to an accepting state (if found).
             *         - The updated list of remaining paths to explore.
             *         Returns None if no accepting state is found.
             */
            @annotation.tailrec
            def heuristicHelper(paths: LazyList[(S, Word[A], Set[S], Set[(Symbol[A], S)])]): Option[(Word[A], LazyList[(S, Word[A], Set[S], Set[(Symbol[A], S)])])] = paths match
                case LazyList() => None
                case (currentState, word, visited, adjacent) #:: rest => 

                    // we can also call if (heuristic(currentState) == 0) meaning that we have reached our goal
                    if (isAccepted(currentState)) Some((word.reverse, rest)) // goal reached
                    else 
                        val nonVisitedAdjacents = adjacent.filterNot((symb, adjState) => visited.contains(adjState))

                        if (nonVisitedAdjacents.isEmpty) heuristicHelper(rest) // no more transitions -> explore other paths
                        else 
                            // Get the adjacent state with the minimum heuristic cost
                            val (newSymbol, newState) = nonVisitedAdjacents.minBy { case (symb, nextState) => heuristic(nextState) } 

                            heuristicHelper(
                                            (newState, newSymbol +: word, visited + newState, getAdjacentStates(newState)) #::  // new state path
                                            (currentState, word, visited, adjacent - ((newSymbol, newState))) #:: // last state path 
                                             rest // other paths
                                            )
            // start exploring with the initial state
            LazyList.unfold(LazyList((dfa.initialState, Seq.empty[Symbol[A]], Set(dfa.initialState), getAdjacentStates(dfa.initialState)))) { paths => heuristicHelper(paths)}

/** Note: Use of LazyList.unfold to generate a lazy sequence of solution paths:
 *  unfold takes an initial paths state (a LazyList containing the initial DFA state, an empty word, 
 *   the set of visited states, and its adjacent states) and a function that generates the next element 
 *   and the new state from it.
 * - At each step, heuristicHelper is called to explore paths. It either:
 *     - Produces a valid word leading to an accepting state (and updates the remaining paths to explore),
 *     - Or signals termination by returning `None` when no more solutions exist.
 * - The resulting LazyList contains words (symbol sequences) leading to accepting states,
 * computed on-demand as the LazyList is traversed.
 * 
 * Consulted References:
 *          - https://livebook.manning.com/book/functional-programming-in-scala-second-edition/chapter-5/v-5/117
 * 
 *          - Manning Functional Programming in Scala, 2nd edition (taken from course)
 *                    Part 1 - Section 5: Strictness and laziness -> Infinite streams and corecursion: unfold
 */   
