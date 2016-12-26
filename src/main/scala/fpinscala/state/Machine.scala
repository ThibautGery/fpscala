package fpinscala.state

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def processCoinInternal: ((Int, Int), Machine)= {
    if(locked) Machine(locked = false, candies, coins + 1).toState
    else this.toState
  }

  def processTurnInternal: ((Int, Int), Machine) = {
    if(candies == 0 || locked) this.toState
    else Machine(locked = true, candies - 1, coins).toState
  }

  private def toState = ((coins, candies), this)
}

object Machine {

  type MachineSimulation = State[Machine, (Int, Int)]

  def processCoin: MachineSimulation = State(_.processCoinInternal)
  def processTurn: MachineSimulation = State(_.processTurnInternal)
  def unit: MachineSimulation = State( machine => ((machine.coins, machine.candies), machine))

  def simulateMachine(inputs: List[Input]): MachineSimulation =  {
    State.sequence(unit :: inputs.map(simulateMachineForOne)).map(_.reverse.head)
  }

  def simulateMachineForOne(input: Input): MachineSimulation =  input match {
    case Coin => processCoin
    case Turn => processTurn
  }

}