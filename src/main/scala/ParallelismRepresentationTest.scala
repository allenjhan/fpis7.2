object ParallelismRepresentationTest {

}

case class Par[A](a: () => A, var output: Option[A])

object Par {

  def unit[A](a: => A): Par[A] =  Par(() => a, None)

  def map2[A, B, C](in1: Par[A], in2: Par[B])(fun: (A, B)=>C): Par[C] = {
    unit(fun(run(in1), run(in2)))
  }

  def fork[A](a: => Par[A]): Par[A] = Par(() => run(a), None)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = {
    val thread = new Thread {
      override def run = {
        a.output = Some(a.a())
      }
    }
    thread.run
    a.output.get
  }

}
