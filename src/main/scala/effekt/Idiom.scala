package effekt

trait Idiom[+A] extends Control[A] {
  def run: A
  def map[B](f: A => B): I[B]
  def map2[B, C](mb: I[B])(f: (A, B) => C): I[C]

  def ap[B](mf: I[A => B]): I[B] = map2(mf) { (a, f) => f(a) }
  def andThen[B](mb: I[B]): I[B] = map2(mb) { (a, b) => b }
  def *>[B](mb: I[B]): I[B] = andThen(mb)
}