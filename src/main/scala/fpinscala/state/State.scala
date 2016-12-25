package fpinscala.state

case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B): State[S, B] = this.flatMap {
    a => State.unit(f(a))
  }

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap {
    a => rb.flatMap {
      b => State.unit(f(a,b))
    }
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(state => {
    val (a, rng2) = run(state)
    val (b, rng3) = g(a).run(rng2)
    (b, rng3)
  })


}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil))(_.map2(_)((x, xs) => x :: xs))
}