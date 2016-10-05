package breeze.symbolic

import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.operators.{OpAdd, OpMulScalar}
import breeze.numerics
import shapeless.LUBConstraint._
import shapeless.{::, <:!<, HList, HNil}

/**
  * Type-class to simplify a SymbolicFunction
  * @tparam F The type of the SymbolicFunction to simplify
  */
trait CanSimplify[-F] {
  type R <: SymbolicFunction[R]
  def simplify(f: F): R
}


trait LowestPriorityCanSimplify {
  implicit def canSimplifyIdentity[F <: SymbolicFunction[F]] = new CanSimplify[F] {
    type R = F
    def simplify(f: F) = f
    override def toString = "Identity"
  }
}

trait LowPriorityCanSimplify extends LowestPriorityCanSimplify {

  implicit def canSimplifyDifference[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]](
    implicit canSimplify1: CanSimplify[F1],
    canSimplify2: CanSimplify[F2]
  ) =
    new CanSimplify[Difference[F1, F2]] {
      type R = Difference[canSimplify1.R, canSimplify2.R]
      def simplify(f: Difference[F1, F2]) = {
        val s1 = canSimplify1.simplify(f.fn1)
        val s2 = canSimplify2.simplify(f.fn2)
        Difference(s1, s2)
      }
    }

  implicit def canSimplifySumHeadAndTail[F, L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit canSimplify: CanSimplify[Sum[L]],
    canSimplifyHead: CanSimplify[F]
  ) =
    new CanSimplify[Sum[F :: L]] {
      type R = Sum[canSimplifyHead.R :: canSimplify.R :: HNil]
      def simplify(f: Sum[F :: L]) = {
        val simplifiedHead = canSimplifyHead.simplify(f.fns.head)
        val simplifiedTail = canSimplify.simplify(Sum(f.fns.tail))
        Sum(simplifiedHead :: simplifiedTail :: HNil)
      }
      override def toString = s"Sum head ($canSimplifyHead) and tail ($canSimplify}"
    }

  implicit def canSimplifySubFunctionOfSingleArgumentFunction[
  I, F[I] <: SingleArgumentFunction[_, F[I]]](
    implicit canSimplify1: CanSimplify[I],
    canBuildTwoArgumentFunction: CanBuildSingleArgumentFunction[I, F]
  ) =
    new CanSimplify[F[I]] {
      type R = F[canSimplify1.R]
      def simplify(f: F[I]) = {
        val s = canSimplify1.simplify(f.fn.asInstanceOf[I])
        canBuildTwoArgumentFunction.build(s)
      }
      override def toString = s"single argument function ($canSimplify1)"
    }

  implicit def canSimplifyProductHeadAndTail[F, L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit canSimplifyHead: CanSimplify[F],
    canSimplifyTail: CanSimplify[Product[L]]
  ) =
    new CanSimplify[Product[F :: L]] {
      type R = Product[canSimplifyHead.R :: canSimplifyTail.R :: HNil]
      def simplify(f: Product[F :: L]) = {
        val simplifiedHead = canSimplifyHead.simplify(f.fns.head)
        val simplifiedTail = canSimplifyTail.simplify(Product(f.fns.tail))
        Product(simplifiedHead :: simplifiedTail :: HNil)
      }
      override def toString = s"Product head ($canSimplifyHead) and tail ($canSimplifyTail)"
    }
}

trait HighPriorityCanSimplify extends LowPriorityCanSimplify {

  implicit val canSimplifyEmptySum = new CanSimplify[Sum[HNil]] {
    type R = ConstZero
    def simplify(f: Sum[HNil]) = ConstZero()
    override def toString = "empty Sum"
  }

  implicit def canSimplifyOneElementSum[F](
    implicit canSimplify: CanSimplify[F]
  ) = new CanSimplify[Sum[F :: HNil]] {
    type R = canSimplify.R
    def simplify(f: Sum[F :: HNil]) = canSimplify.simplify(f.fns.head)
    override def toString = s"1 element Sum ($canSimplify)"
  }

  implicit def canSimplifyZeroPlusL[L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit canSimplify: CanSimplify[Sum[L]]) =
    new CanSimplify[Sum[ConstZero :: L]] {
      type R = canSimplify.R
      def simplify(f: Sum[ConstZero :: L]) = canSimplify.simplify(Sum(f.fns.tail))
      override def toString = s"Sum 0 + tail ($canSimplify)"
    }

  implicit def canSimplifyFPlusZero[F](
    implicit canSimplify: CanSimplify[F]
  ) =
    new CanSimplify[Sum[F :: ConstZero :: HNil]] {
      type R = canSimplify.R
      def simplify(f: Sum[F :: ConstZero :: HNil]) = {
        canSimplify.simplify(f.fns.head)
      }
      override def toString = s"Sum ($canSimplify) + 0"
    }

  implicit def canSimplifyConstPlusConst[T, L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit add: OpAdd.Impl2[T, T, T],
    canSimplify: CanSimplify[Sum[L]]
  ) =
    new CanSimplify[Sum[Const[T] :: Const[T] :: L]] {
      type R = Sum[Const[T] :: canSimplify.R :: HNil]
      def simplify(f: Sum[Const[T] :: Const[T] :: L]) = {
        val headConst = Const(add(f.fns.head.const, f.fns.tail.head.const))
        val simplifiedTail = canSimplify.simplify(Sum(f.fns.tail.tail))
        Sum(headConst :: simplifiedTail :: HNil)
      }
      override def toString = s"Sum const + const + tail ($canSimplify)"
    }

  implicit def canSimplifyOneTimesF[F <: SymbolicFunction[F], L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit canSimplify: CanSimplify[Product[F :: L]]) =
    new CanSimplify[Product[ConstOne :: F :: L]] {
      type R = canSimplify.R
      def simplify(f: Product[ConstOne :: F :: L]) =
        canSimplify.simplify(Product(f.fns.tail))
      override def toString = s"1 * tail ($canSimplify}"
    }

  implicit def canSimplifyFTimesOne[F <: SymbolicFunction[F], L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit canSimplify: CanSimplify[Product[F :: L]],
    // Need to XOR this with other implicits
    fIsNotOne: F <:!< ConstOne
  ) =
    new CanSimplify[Product[F :: ConstOne :: L]] {
      type R = canSimplify.R
      def simplify(f: Product[F :: ConstOne :: L]) =
        canSimplify.simplify(Product(f.fns.head :: f.fns.tail.tail))
      override def toString = s"(head * 1 * tail) ($canSimplify)"
    }

  implicit def canSimplifyZeroTimesF[I, O, L <: HList : <<:[SymbolicFunction[_]]#λ](
    // Need to XOR this with other implicits
    implicit fIsNotOne: L <:!< ConstOne
  ) =
    new CanSimplify[Product[ConstZero :: L]] {
      type R = ConstZero
      def simplify(f: Product[ConstZero :: L]) = ConstZero()
      override def toString = "0 * x"
    }

  implicit def canSimplifyFTimesZero[F, L <: HList : <<:[SymbolicFunction[_]]#λ](
    // Need to XOR this with other implicits
    implicit fIsNotOne: F <:!< ConstOne,
    fIsNotZero: F <:!< ConstZero
  ) =
    new CanSimplify[Product[F :: ConstZero :: L]] {
      type R = ConstZero
      def simplify(f: Product[F :: ConstZero :: L]) = ConstZero()
      override def toString = "x * 0"
    }

  implicit def canSimplifyConstTimesConst[T, L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit multiply: OpMulScalar.Impl2[T, T, T],
    canSimplify: CanSimplify[Product[L]]
  ) =
    new CanSimplify[Product[Const[T] :: Const[T] :: L]] {
      type R = Product[Const[T] :: canSimplify.R :: HNil]
      def simplify(f: Product[Const[T] :: Const[T] :: L]) =
        Product(
          Const(multiply(f.fns.head.const, f.fns.tail.head.const)) :: canSimplify
            .simplify(Product(f.fns.tail.tail)) :: HNil)
    }

  implicit val canSimplifyEmptyProduct = new CanSimplify[Product[HNil]] {
    type R = ConstOne
    def simplify(f: Product[HNil]) = ConstOne()
    override def toString = "Empty Product"
  }
  implicit def canSimplifyOneElementProduct[F](
    implicit canSimplify: CanSimplify[F]
  ) = new CanSimplify[Product[F :: HNil]] {
    type R = canSimplify.R
    def simplify(f: Product[F :: HNil]) = canSimplify.simplify(f.fns.head)
    override def toString = s"One Element Product ($canSimplify)"
  }

  implicit val canSimplifyExpZero = new CanSimplify[Exponential[ConstZero]] {
    type R = ConstOne
    def simplify(f: Exponential[ConstZero]) = ConstOne()
  }

  implicit def canSimplifyExpConst[T](exp: numerics.exp.Impl[T, T]) =
    new CanSimplify[Exponential[Const[T]]] {
      type R = Const[T]
      def simplify(f: Exponential[Const[T]]): R = Const(exp(f.fn.const))
    }

  implicit def canSimplifyProductOfExps[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2], L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit canSimplify: CanSimplify[L]
  ) =
    new CanSimplify[Product[Exponential[F1] :: Exponential[F2] :: L]] {
      type R = Product[
        Exponential[Sum[F1 :: F2 :: HNil]] :: canSimplify.R :: HNil]
      def simplify(f: Product[Exponential[F1] :: Exponential[F2] :: L]): R =
        Product(
          Exponential(Sum(f.fns.head.fn :: f.fns.tail.head.fn :: HNil)) :: canSimplify
            .simplify(f.fns.tail.tail) :: HNil)
    }

  implicit def canSimplifyExpLog[F <: SymbolicFunction[F]] =
    new CanSimplify[Exponential[Logarithm[F]]] {
      type R = F
      def simplify(f: Exponential[Logarithm[F]]) = f.fn.fn
    }

  implicit def canSimplifyLogExp[F <: SymbolicFunction[F]] =
    new CanSimplify[Logarithm[Exponential[F]]] {
      type R = F
      def simplify(f: Logarithm[Exponential[F]]) = f.fn.fn
    }

  implicit val canSimplifyLogOne = new CanSimplify[Logarithm[ConstOne]] {
    type R = ConstZero
    def simplify(f: Logarithm[ConstOne]) = ConstZero()
  }

  implicit def canSimplifyLogConst[T](log: numerics.log.Impl[T, T]) =
    new CanSimplify[Logarithm[Const[T]]] {
      type R = Const[T]
      def simplify(f: Logarithm[Const[T]]): R = Const(log(f.fn.const))
    }
}

object CanSimplify extends HighPriorityCanSimplify {
  implicit class RichSimplifiable[F](f: F) {
    def simplify(implicit canSimplify: CanSimplify[F]): canSimplify.R =
      canSimplify.simplify(f)
  }
}

object simplify extends UFunc with MappingUFunc {
  implicit def implCanSimplify[F <: SymbolicFunction[F]](implicit canSimplify: CanSimplify[F]): Impl[F, canSimplify.R] = new Impl[F, canSimplify.R] {
    def apply(f: F) = canSimplify.simplify(f)
  }
}