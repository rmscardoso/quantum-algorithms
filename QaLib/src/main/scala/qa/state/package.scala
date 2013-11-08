package qa

package object state {

  class Coefficient(coefficient: Double) {
    def *(state: State): State =
      state * coefficient
  }

  implicit def *(d: Double): Coefficient =
    new Coefficient(d)

}