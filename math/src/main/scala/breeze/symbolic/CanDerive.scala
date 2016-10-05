package breeze.symbolic

import breeze.generic.{MappingUFunc, UFunc}
import shapeless.LUBConstraint._
import shapeless.{::, HList, HNil}

/**
  * Type class to calculate the symbolic derivative of a SymbolicFunction
  * @tparam F The type of the SymbolicFunction to derive
  */
trait CanDerive[F <: SymbolicFunction[F]] {
  type D <: SymbolicFunction[D]
  def derivation(f: F): D
}

trait LowPriorityCanDerive {
  implicit def derivableConst[F <: ConstBase[F]] =
    new CanDerive[F] {
      type D = ConstZero
      def derivation(f: F) = ConstZero()
    }
  implicit def derivableEmptySum =
    new CanDerive[Sum[HNil]] {
      type D = ConstZero
      def derivation(p: Sum[HNil]) =
        ConstZero()
    }
  implicit def derivableSum[F <: SymbolicFunction[F], L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit d1: CanDerive[F], d2: CanDerive[Sum[L]]) =
    new CanDerive[Sum[F :: L]] {
      type D = Sum[d1.D :: d2.D :: HNil]
      def derivation(f: Sum[F :: L]) =
        Sum(d1.derivation(f.fns.head) :: d2.derivation(Sum(f.fns.tail)) :: HNil)
    }

  implicit def derivableDifference[V <: SymbolicFunction[V], S <: SymbolicFunction[S]](
    implicit dv: CanDerive[V], ds: CanDerive[S]) =
    new CanDerive[Difference[V, S]] {
      type D = Difference[dv.D, ds.D]
      def derivation(f: Difference[V, S]) =
        Difference(dv.derivation(f.fn1), ds.derivation(f.fn2))
    }
  implicit def derivableEmptyProduct =
    new CanDerive[Product[HNil]] {
      type D = ConstZero
      def derivation(p: Product[HNil]) =
        ConstZero()
    }
  implicit def derivableProduct[F <: SymbolicFunction[F], L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit df1: CanDerive[F],
    df2: CanDerive[Product[L]]
  ) =
    new CanDerive[Product[F :: L]] {
      type D = Sum[Product[F :: df2.D :: HNil] :: Product[df1.D :: L] :: HNil]
      def derivation(p: Product[F :: L]) = {
        val ld = df2.derivation(Product(p.fns.tail))
        Sum(Product(p.fns.head :: ld :: HNil) ::
          Product(df1.derivation(p.fns.head) :: p.fns.tail) :: HNil)
      }
    }

  implicit def derivableDivision[NOM <: SymbolicFunction[NOM], DIV <: SymbolicFunction[DIV]](
    implicit dn: CanDerive[NOM], dd: CanDerive[DIV]) =
    new CanDerive[Division[NOM, DIV]] {
      type D = Difference[
        Division[dn.D, DIV],
        Division[Product[NOM :: dd.D :: HNil], Product[DIV :: DIV :: HNil]]]

      /**
        * (g / h)' = (g' * h - g * h') / h^2 = g' / h - g * h' / h^2
        */
      def derivation(f: Division[NOM, DIV]) =
        Difference(Division(dn.derivation(f.fn1), f.fn2),
          Division(Product(f.fn1 :: dd.derivation(f.fn2) :: HNil),
            Product(f.fn2 :: f.fn2 :: HNil)))
    }

  implicit def derivableExponential[F <: SymbolicFunction[F]](implicit df: CanDerive[F]) =
    new CanDerive[Exponential[F]] {
      type D = Product[df.D :: Exponential[F] :: HNil]
      def derivation(f: Exponential[F]) =
        Product(df.derivation(f.fn) :: f :: HNil)
    }


  implicit def derivableLogarithm[F <: SymbolicFunction[F]](implicit df: CanDerive[F]) =
    new CanDerive[Logarithm[F]] {
      type D = Division[df.D, F]
      def derivation(f: Logarithm[F]) =
        Division(df.derivation(f.fn), f.fn)
    }

  implicit def derivableVar = new CanDerive[Var] {
    type D = ConstOne
    def derivation(v: Var) = ConstOne()
  }

  implicit def derivableIdentity[T] = new CanDerive[Identity[T]] {
    type D = ConstOne
    def derivation(f: Identity[T]) = ConstOne()
  }

  implicit def derivableChain[H <: SymbolicFunction[H], G <: SymbolicFunction[G]](
    implicit dh: CanDerive[H], dg: CanDerive[G]) =
    new CanDerive[Chain[H, G]] {
      type D = Product[Chain[H, dg.D] :: dh.D :: HNil]

      /**
        * g(h)' = g'(h) * h'
        */
      def derivation(f: Chain[H, G]) = {
        val firstDerivation = dh.derivation(f.firstFunc)
        val secondDerivation = dg.derivation(f.secondFunc)
        Product(
          Chain(f.firstFunc, secondDerivation) :: firstDerivation :: HNil)
      }
    }
}

object CanDerive extends LowPriorityCanDerive {
  def apply[F <: SymbolicFunction[F]](implicit derivable: CanDerive[F]) = derivable
  type Aux[F <: SymbolicFunction[F], D0 <: SymbolicFunction[D0]] = CanDerive[F] { type D = D0 }
  case class SingletonOf[T, U <: { type D }](widen: T { type D = U#D })
  object SingletonOf {
    implicit def mkSingleton[T <: {type D}](implicit t: T) =
      SingletonOf[T, t.type](t)
  }
  implicit class RichDerivable[F <: SymbolicFunction[F]](f: F) {
    def derivation(implicit canDerive: CanDerive[F]): canDerive.D = canDerive.derivation(f)
  }
}

object derive extends UFunc with MappingUFunc {
  implicit def implCanDerive[F <: SymbolicFunction[F]](implicit canDerive: CanDerive[F]): Impl[F, canDerive.D] = new Impl[F, canDerive.D] {
    def apply(f: F) = canDerive.derivation(f)
  }
}