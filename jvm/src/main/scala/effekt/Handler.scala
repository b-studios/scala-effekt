package effekt

private[effekt]
trait Handler extends Serializable { outer =>

  val prompt: Capability
  val state: prompt.effect.State
  type Res = prompt.Res

  def updateWith(s: prompt.effect.State): Handler {val prompt: outer.prompt.type} =
    new Handler {
      val prompt: outer.prompt.type = outer.prompt
      val state = s
    }
}
