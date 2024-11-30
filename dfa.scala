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
             * @param stack a list of paths in the form of tuples: (current state, word, set of visited states, set of adjacents of the current state)
             * @param solution current solution 
             * 
             * @return a list of words leading to a solution
             */
            @annotation.tailrec
            def solveHelper(stack: List[(S, Word[A], Set[S], Set[(Symbol[A], S)])], solution: List[Word[A]]): List[Word[A]] = stack match 
                case Nil => solution // All paths explored, return solutions
                case (currentState, word, visited, adjacent) :: rest => 
                    if (adjacent.isEmpty)  // No more transitions 

                        // We can check if the word is accepted : efficiency: we have to recompute transitions, which is not too efficient
                        // reversedWord = word.reverse
                        // if (accept(reversedWord)) solveHelper(rest, reversedWord :: solution) // Add the accepted word to solution

                        // Check if the current state is an accepting state : We are using this one for more efficiency ! 
                        if (isAccepted(currentState)) solveHelper(rest, word.reverse :: solution) // Add the word leading to this accepting state
                        else solveHelper(rest, solution) // Explore other paths

                    else 
                        // Explore the next transition
                        val (symbol, nextState) = adjacent.head
                        val remainingAdjacent = adjacent.tail

                        if (visited.contains(nextState)) 
                            // Skip already visited states
                            solveHelper((currentState, word, visited, remainingAdjacent) :: rest, solution)
                        else 
                            // Build the new state
                            val newWord = symbol +: word
                            val newVisited = visited + nextState
                            val newAdjacent = getAdjacentStates(nextState)

                            // Add new state exploration to stack
                            solveHelper(
                                (nextState, newWord, newVisited, newAdjacent) :: 
                                (currentState, word, visited, remainingAdjacent) :: 
                                rest, 
                                solution
                            )
                        

            // Start with the initial state
            solveHelper(List((dfa.initialState, Nil, Set(dfa.initialState), getAdjacentStates(dfa.initialState))), Nil)

        /** This lazySolve method gets all possible words leading to an acyclic solution path for the DFA starting at initial state
         * Returns a lazy list of words leading initial state to an accepting state (on demand)
         * 
         * @return LazyList[Word[A]]: a lazy list of words leading to a solution
         */
        def lazySolve(): LazyList[Word[A]] = 
            @annotation.tailrec
            /** lazyHelper function gets all possible words leading to "on demand" solutions (acyclic paths) for a DFA starting at a given state
             * Returns a lazy list of words leading a given state to an accepting state 
             * 
             * @param stack a lazy list of paths in the form of tuples: (current state, word, set of visited states, set of adjacents of the current state)
             * @param solution current solution 
             * 
             * @return a lazy list of words leading to a solution 
             */
            def lazyHelper(stack: LazyList[(S, Word[A], Set[S], Set[(Symbol[A], S)])], solution: LazyList[Word[A]]): LazyList[Word[A]] = stack match 
                case LazyList() => solution
                case (currentState, word, visited, adjacent) #:: rest => 
                    if (adjacent.isEmpty)

                        // Same here, we can check if the word is accepted. Efficiency: we have to recompute transitions, which is not too efficient
                        // val reversedWord = word.reverse
                        // if (accept(reversedWord)) lazyHelper(rest, reversedWord #:: solution)

                        // Check if the current state is an accepting state : We are using this one for more efficiency ! 
                        if (isAccepted(currentState)) lazyHelper(rest, word.reverse #:: solution) // Add the word leading to this accepting state
                        else lazyHelper(rest, solution)

                    else 
                        val (symbol, newState) = adjacent.head
                        val remainingAdjacent = adjacent.tail
                        
                        if (visited.contains(newState)) 
                            lazyHelper((currentState, word, visited, remainingAdjacent) #:: rest, solution)

                        else 
                            val newWord = symbol +: word
                            val newVisited = visited + newState
                            val newAdjacent = getAdjacentStates(newState)

                            lazyHelper(
                                (newState, newWord, newVisited, newAdjacent) #:: 
                                (currentState, word, visited, remainingAdjacent) #:: 
                                rest, 
                                solution
                            )

            lazyHelper(LazyList((dfa.initialState, Nil, Set(dfa.initialState), getAdjacentStates(dfa.initialState))), LazyList.empty)

        // solve heuristique -- ! n'est pas 100% correct car mauvaise utilisation de lazylist !
        def heuristicSolve(heuristic: S => Double): LazyList[Word[A]] = 
            @annotation.tailrec
            def heuristicHelper(paths: LazyList[(S, Word[A], Set[S], Set[(Symbol[A], S)])], solution: LazyList[Word[A]]): LazyList[Word[A]] = paths match
                case LazyList() => solution
                case (currentState, word, visited, adjacent) #:: rest => 
 
                    if (isAccepted(currentState)) {println("found solution"); heuristicHelper(rest, word.reverse #:: solution)} // génère toutes les solutions
                    // if (isAccepted(currentState)) {println("found solution");  word.reverse #:: solution} // pour une seule solution
                    else 
                        if (adjacent.isEmpty)
                            heuristicHelper(rest, solution)
                        else 
                            val newAdjacent = adjacent.filterNot((adjSymb, adjState) => visited.contains(adjState)) // exclure les etats visités

                            if (newAdjacent.isEmpty) heuristicHelper(rest, solution)
                            else 
                                val (newSymbol, newState) = newAdjacent.minBy { case (_, nextState) => heuristic(nextState) } // choisir l'etat avec le cout minimum
                                val newPaths = (newState, newSymbol +: word, visited + newState, getAdjacentStates(newState)) #:: // ajouter un nouveau chemin
                                ((currentState, word, visited, adjacent - ((newSymbol, newState))) #:: rest)

                                heuristicHelper(newPaths, solution)

            heuristicHelper(LazyList((dfa.initialState, Nil, Set(dfa.initialState), getAdjacentStates(dfa.initialState))), LazyList()) // appel initial

/* 
 Explication de la fonction solve: elle prend 2 arguments :
    1) stack: Une liste contenant le mot a construire a partir d'un etat, un ensemble d'états visités pour un chemin, et un ensemble d'états adjacents a partir d'un état.
        Le stack sera utilisé pour stocker les chemins menant à une solution

    2) solution: Une liste contenant tous les mots conduisant a un etat accepteur.

    Algorithme:     
        Cas de base: Le stack est vide -> Il n'y a plus aucun chemin à explorer, on retourne la liste des solutions
        Cas de recursion: Le stack n'est pas vide -> il y a encore des chemins à explorer   (on prend le premier chemin du stack)

                                                     Si aucun état adjacent est possible -> le chemin choisi est terminé, car aucune transition est possible, 
                                                                                            on verifie si le mot construit dans ce chemin mène à un état accepteur (fonction accept)
                                                                                            Oui? On ajoute ce mot dans la liste de solutions, et on continue la recherche avec le reste du stack (autres chemins)
                                                                                            Non? On continue la recherche avec le reste du stack (autres chemins)
                                                    
                                                     Si présence d'états adjacents       -> On choisit un état adjacent, on vérifie si cet état a été visité 
                                                                                            Si oui, on passe à l'etat adjacent suivant
                                                                                            Si non, on ajoute un nouveau chemin partant de cet etat, et on continue la recherche (en mettant à jour l'ensemble des etats visités)
                                                                    

    On commence toujours par l'etat initial (dont on cosidere un état visité)
 */
