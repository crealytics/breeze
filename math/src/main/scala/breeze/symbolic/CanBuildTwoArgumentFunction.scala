package breeze.symbolic

trait CanBuildTwoArgumentFunction[F[_ <: SymbolicFunction, _ <: SymbolicFunction] <: SymbolicFunction] {
  def build[F1 <: SymbolicFunction, F2 <: SymbolicFunction](f1: F1, f2: F2): F[F1, F2]
}

object CanBuildTwoArgumentFunction {

  implicit def canBuildDivision = new CanBuildTwoArgumentFunction[Division] {
    override def build[F1 <: SymbolicFunction, F2 <: SymbolicFunction](f1: F1, f2: F2): Division[F1, F2] =
      Division(f1, f2)
  }
  implicit def canBuildDifference =
    new CanBuildTwoArgumentFunction[Difference] {
      override def build[F1 <: SymbolicFunction, F2 <: SymbolicFunction](f1: F1, f2: F2): Difference[F1, F2] =
        Difference(f1, f2)
    }

}