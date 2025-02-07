# Functional Programming Project 2024-2025  

## Overview  
This project implements a **Deterministic Finite Automaton (DFA)** solver to analyze and solve puzzles. A DFA is defined by a tuple **(Q, Σ, δ, s, F)**, where:  
- **Q**: Finite set of states  
- **Σ**: Finite alphabet  
- **δ**: Transition function **Q × Σ → Q**  
- **s**: Initial state  
- **F**: Set of accepting states  

## Key Features  
- **`accept` Function**: Checks if a word is accepted by the DFA.  
- **`solve` Function**: Finds all **acyclic words** leading to an accepting state.  
- **Lazy Evaluation (`lazysolve`)**: Generates solutions on demand.  

## Implemented Problems  
1. **Odd Binary Strings**: Accepts binary strings with an odd number of `1`s.  
2. **Wolf, Goat & Cabbage Puzzle**: Models a farmer's safe transport across a river.  
3. **15-Puzzle (Sliding Puzzle)**: Finds valid move sequences to reach the solved state.  

## Compilation & Execution  
### Prerequisites  
- **Scala** installed (`scalac` available)  

### Steps  
1. Compile the project:  
   ```sh
   scalac dfa.scala  
   ```  
2. Run the program:  
   ```sh
   scala dfa 
   ```  

## Example Usage  
```scala
val dfa = DFA(states, alphabet, transition, initialState, acceptingStates)
dfa.accept(List(1,1,1,1))  // true
dfa.solve()  // List of valid sequences
```

## Contributors  
- [Mehdi Boustani](https://github.com/MehdiBoustani)
- [Alexandre Bourguignon](https://github.com/BourguiBinks)
- [Abdelkader Albashityalshaier](https://github.com/AbdelkaderULG) 
