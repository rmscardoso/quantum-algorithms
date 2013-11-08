package qa.operator

import qa.state.State

object CNotTransitions {
  
  def transitions(controlling: List[Int], controlled: Int) = {
    def max = (controlled :: controlling).max
    def states = allQubitStates(max + 1)
    var stateTransitions = Map.empty[State, State]
    for(from <- states) {
      var shouldSwap = true
      for(c <- controlling) {
        if(0 == from(c)) {
          shouldSwap = false
        }
      }
      
      if(shouldSwap) {
    	val toState = collection.mutable.ListBuffer[Int]() ++ from
        toState(controlled) = if (0 == from(controlled)) 1 else 0
        stateTransitions += (State(from) -> State(toState.toList))
      } else {
        stateTransitions += (State(from) -> State(from))
      }
    }
    stateTransitions
  }
  
  def allQubitStates(count: Int) = {
    (for(i <- 0 until math.pow(2, count).intValue) yield bitList(i, count)).toList
  }
  
  def bitList(value: Int, dim: Int) = {
    var l = List.empty[Int]
    var remaining = value
    for(i <- 0 until dim) {
      l = (if(0 == remaining % 2) 0 else 1) :: l
      remaining /= 2
    }
    l
  }
  
}