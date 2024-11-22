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
    val s15 = State(Set(), Set(P, L, M, C)) // Accepter 

    private def isSink(state: State[Item]): Boolean = {

        // Helper function to check a specific condition on a side
        def hasConflict(side: Set[Item], item1: Item, item2: Item): Boolean = 
            side.contains(item1) && side.contains(item2) && !side.contains(P)

        // Check for conflicts on both sides
        val wolfGoatConflict = hasConflict(state.left, M, L) || hasConflict(state.right, M, L)
        val goatCabbageConflict = hasConflict(state.left, M, C) || hasConflict(state.right, M, C)

        wolfGoatConflict || goatCabbageConflict // True if sink
    }


    private def isValid(state: State[Item], symbol: Char): Boolean = {
        def isTogether(item: Item): Boolean =
            (state.left.contains(item) && state.left.contains(P)) ||
            (state.right.contains(item) && state.right.contains(P))

        symbol match
            case 'p' => true
            case 'l' => isTogether(L)
            case 'm' => isTogether(M)
            case 'c' => isTogether(C)
            case _   => false
    }

    private def moveNext(state: State[Item], symbol: Char): State[Item] = {
        def move(item: Item): State[Item] = {
            if (state.left.contains(P) && state.left.contains(item))
                State(state.left - P - item, state.right + P + item)
            else
                State(state.left + P + item, state.right - P - item)
        }

        symbol match
            case 'p' => move(P)
            case 'l' => move(L)
            case 'm' => move(M)
            case 'c' => move(C)
            case _   => state // Si symbol est invalide, retourne l'état initial
    }

    case class plmcDFA() extends DFA[State[Item], Char]:
        override def states = Set(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)
        override def alphabet = Set('p', 'l', 'm', 'c')
        override def initialState = s0
        override def acceptingStates = Set(s15)

        override def transition(state: State[Item], symbol: Char): Option[State[Item]] = isValid(state, symbol) match
            case false => None
            case true => 
                val nextState = moveNext(state, symbol) 
                if !isSink(nextState) then Some(nextState) else None