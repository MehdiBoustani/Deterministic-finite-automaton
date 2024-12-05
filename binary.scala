import ObjectDFA._

/** Deterministic Finite Automaton to accept binary strings with an odd number of `1`s.
  * The DFA transitions between two states: 'even' and 'odd', based on the number of `1`s encountered.
  */
object BinaryOddDFA:

    /** Represents a state in the DFA with a name, either "even" or "odd", 
      * indicating whether the number of `1`s in the string is even or odd, respectively.
      */
    case class BinaryState(name: String) extends StateDFA

    val even = BinaryState("even") // State where the number of `1`s is even
    val odd = BinaryState("odd") // State where the number of `1`s is odd

    /* DFA implementation for accepting binary strings with an odd number of `1`s. */
    case class BinaryOddDFA() extends DFA[BinaryState, Char]:
        override def states: Set[BinaryState] = Set(even, odd)

        /** The alphabet of the DFA, consisting of the symbols `0` and `1` for binary strings. */
        override def alphabet: Set[Symbol[Char]] = Set('0', '1')

        /** The initial state of the DFA, where the number of `1`s is assumed to be even (empty string). */
        override def initialState: BinaryState = even

        /** The set of accepting states of the DFA. 
          * The DFA accepts strings where the number of `1`s is odd, i.e., the 'odd' state.
          */
        override def acceptingStates: Set[BinaryState] = Set(odd)

        /** Defines the transition function of the DFA, which specifies how the state changes based on the current state 
          * and the input symbol ('0' or '1').
          *
          * @param state The current state of the DFA ('even' or 'odd').
          * @param symbol The input symbol ('0' or '1').
          * @return The next state after applying the transition, if valid.
          */
        override def transition(state: BinaryState, symbol: Char): Option[BinaryState] = (state, symbol) match
            case (`even`, '0') => Some(even)
            case (`even`, '1') => Some(odd)
            case (`odd`, '0') => Some(odd)
            case (`odd`, '1') => Some(even)
            case _           => None
    
    /**
      * Main entry point of the application.
      *
      * @param args Command-line arguments (not used in this implementation).
      */
    def main(args: Array[String]): Unit = {
        // Instantiate the DFA for binary strings with an odd number of `1`s.
        val dfa = new BinaryOddDFA()

        // Display the initial state (should be 'even', as no `1`s have been encountered yet).
        println(s"Initial state: ${dfa.initialState.name}")

        // solve: always returns one solution: "1",  because no cycles and we start from even 
        println(s"Solve result: ${dfa.solve().mkString}")  
    
        // Accept test for 10101011 and 10101010
        val testStrings = List("10101011", "10101010")
        testStrings.foreach { testString =>
            // Print the result of accepting or rejecting each test string.
            println(s"Accept test for '${testString}': ${dfa.accept(testString)}")
        }
    }