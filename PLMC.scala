import ObjectDFA._

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

    case class State[Q](left: Set[Q], right: Set[Q]) extends StateDFA

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
    val s15 = State(Set(), Set(P, L, M, C)) // Accepting state


    /** This isSink function checks if a given state is a "sink state" where it is impossible 
    * to proceed further (i.e. safety constraints of the problem have been violated).
    *  
    * @param state: a given state
    * 
    * @return true if state is a sink state
    *         false otherwise
    */
    private def isSink(state: State[Item]): Boolean = 
        /** Helper function to check for conflicts on same side of the river.
         *  A conflict occurs when two specific items (item1 and item2) are on the same side of the river 
         *  without the Farmer (P) to supervise them. 
         *
         * @param side a side (set of Item) 
         * @param item1 an item 
         * @param item2 an item
         * 
         * @return true if a conflict has occured
         *         false otherwise
        */
        def hasConflict(side: Set[Item], item1: Item, item2: Item): Boolean = 
            side.contains(item1) && side.contains(item2) && !side.contains(P)

        hasConflict(state.left, M, L) || hasConflict(state.right, M, L) || // Wolf and Goat conflict
        hasConflict(state.left, M, C) || hasConflict(state.right, M, C)    // Gabbage and Goat conflict

    /** This isValid function checks if a given symbol can lead a transition for a given state 
     * 
     * @param state a state  
     * @param symbol a symbol
     * 
     * @return true if a transition is possible using the symbol
     *         false otherwise
     */
    private def isValid(state: State[Item], symbol: Char): Boolean = 
        /** Helper function to check if a given item is with the farmer on same side
         *
         * @param item: an item
         * 
         * @return true if the given item is on same side of the farmer 
         *         false otherwise  
         */
        def isWithFarmer(item: Item): Boolean =
            (state.left.contains(item) && state.left.contains(P)) ||
            (state.right.contains(item) && state.right.contains(P))

        symbol match
            case 'p' => true
            case 'l' => isWithFarmer(L)
            case 'm' => isWithFarmer(M)
            case 'c' => isWithFarmer(C)
            case _   => false
    
    /** This moveNext function allows to move to another state using a symbol
     * 
     * @param state current state  
     * @param symbol a symbol
     * 
     * @return new state: State[Item]
     */
    private def moveNext(state: State[Item], symbol: Char): State[Item] = 
        /**
          * Helper function that creates the new state given an item
          *
          * @param item an item
          * @return new state: State[Item] 
          */
        def move(item: Item): State[Item] = 
            if (state.left.contains(P) && state.left.contains(item))
                State(state.left - P - item, state.right + P + item)
            else
                State(state.left + P + item, state.right - P - item)
        
        symbol match
            case 'p' => move(P)
            case 'l' => move(L)
            case 'm' => move(M)
            case 'c' => move(C)
            case _   => state // Invalid symbol -> stay in the same state

    /**
      * Defining a DFA for plmc problem
      *
      * @param initState initial state (by default all the items are on the left side)
      */
    case class PlmcDFA(initState: State[Item] = s0) extends DFA[State[Item], Char]:
        override def states = Set(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)
        override def alphabet = Set('p', 'l', 'm', 'c')
        override def initialState = s0
        override def acceptingStates = Set(s15)

        /** This transition method allows to make a transition from a state using a symbol
        * 
        * @param state current state  
        * @param symbol a symbol
        * 
        * @return Option[State[Item]]:
        *               Some(new state) if new state is valid and not sink
        *               None otherwise     
        */
        override def transition(state: State[Item], symbol: Char): Option[State[Item]] = isValid(state, symbol) match
            case false => None
            case true => 
                val nextState = moveNext(state, symbol) 
                if !isSink(nextState) then Some(nextState) else None


    /**
      * Main entry point of the application.
      *
      * @param args Command-line arguments (not used in this implementation).
    */
    def main(args: Array[String]): Unit = {

        // define our DFA
        val plmcDFA = PlmcDFA()
        
        // solve 
        println("solve solutions: " + plmcDFA.solve().map(solution => solution.mkString))

        // accept test for mpcmlcmlcmlpm and ppp
        val testList = List("mpcmlcmlcmlpm", "ppp")
        val results = testList.map(word => s"Accept test for '$word': ${plmcDFA.accept(word)}")
        println(results.mkString("\n"))
       
    }