package breeze.symbolic

trait CanBuildSingleArgumentFunction[F[_ <: SymbolicFunction] <: SymbolicFunction] {
  def build[I <: SymbolicFunction](i: I): F[I]
}

object CanBuildSingleArgumentFunction {
  implicit def canBuildExponential =
    new CanBuildSingleArgumentFunction[Exponential] {
      override def build[I <: SymbolicFunction](i: I): Exponential[I] = new Exponential(i)
    }

  implicit def canBuildLogarithm =
    new CanBuildSingleArgumentFunction[Logarithm] {
      override def build[I <: SymbolicFunction](i: I): Logarithm[I] = new Logarithm(i)
    }

}