import scala.scalajs.js

package object effekt extends EffektApi {
  type ->[-A, +B] = js.Function1[A, B]
}