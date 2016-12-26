package fpinscala.state

import org.specs2.mutable.Specification


class MachineTest extends Specification {
  "process coin" should {
    "do nothing if the machine is already unlock" in {
      val machine = Machine(locked = false, 10, 0)
      machine.processCoinInternal must_== ((0, 10), machine)
    }

    "unlock the machine and store one coin if it is lock" in {
      val machine = Machine(locked = true, 10, 0)
      machine.processCoinInternal must_== ((1, 10), Machine(locked = false, 10, 1))
    }
  }

  "process turn" should {
    "do nothing if the machine is lock" in {
      val machine = Machine(locked = true, 10, 0)
      machine.processTurnInternal must_== ((0, 10), machine)
    }

    "do nothing if they is no more candy" in {
      val machine = Machine(locked = false, 0, 10)
      machine.processTurnInternal must_== ((10, 0), machine)
    }

    "dispense a candy and lock the machine if it is unlock" in {
      val machine = Machine(locked = false, 10, 1)
      machine.processTurnInternal must_== ((1, 9), Machine(locked = true, 9, 1))
    }
  }

  "simulateMachine" should {
    "do nothing is no input" in {
      val machine = Machine(locked = true, 5, 10)
      Machine.simulateMachine(Nil).run(machine) must_== ((10, 5), machine)
    }

    "process input" in {
      val machine = Machine(locked = true, 5, 10)
      Machine.simulateMachine(List(Coin, Turn, Coin, Turn)).run(machine) must_== ((12, 3), Machine(locked = true, 3, 12))
    }
  }

  "modify machine state" should {
    "update the state" in {
      val machine = Machine(locked = true, 0, 0)
      State.modify[Machine]( m => {
        Machine(!m.locked, m.candies, m.coins)
      }).run(machine) must_== ((), Machine(locked = false, 0, 0))
    }
  }
}
