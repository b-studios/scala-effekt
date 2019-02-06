package effekt

trait Control[+A] {
  private[effekt] def run: A
  def map[B](f: A => B): C[B]
  def flatMap[B](f: A => C[B]): C[B]
  def map2[B, D](mb: C[B])(f: (A, B) => D): C[D] = for {
    a <- this
    b <- mb
  } yield f(a, b)

  def ap[B](mf: C[A => B]): C[B] = map2(mf) { (a, f) => f(a) }
  def andThen[B](mb: C[B]): C[B] = map2(mb) { (a, b) => b }
  def *>[B](mb: C[B]): C[B] = andThen(mb)
}
