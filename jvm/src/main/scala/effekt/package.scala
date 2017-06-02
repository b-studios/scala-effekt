package object effekt extends EffektApi {

  private[effekt]
  type H[C <: Capability] = Handler { val prompt: C }

  private[effekt]
  type Frame[-A, +B] = A => Control[B]

}