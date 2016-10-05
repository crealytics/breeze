package breeze.symbolic

import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.DenseVector
import shapeless.labelled.FieldType
import shapeless.ops.hlist.IsHCons
import shapeless.ops.record.Keys
import shapeless.{::, HList, HNil, LUBConstraint, Const => _}

import scala.reflect.ClassTag

/**
  * Type-class to lift a sequence of scalar SymbolicFunctions to a vectorized representation
  * @tparam E The type of the scalar SymbolicFunction
  */
trait CanVectorize[-E] {
  type V
  def lift(fs: Seq[E]): V
}

trait LowestPriorityCanLift {
  implicit def wrapSymbolicFunctions[F <: SymbolicFunction[F]] = new CanVectorize[F] {
    type V = VectorizedSymbolicFunction[F]
    override def lift(fs: Seq[F]): VectorizedSymbolicFunction[F] = {
      println(s"Couldn't lift $fs to anything better than VectorizedSymbolicFunction")
      VectorizedSymbolicFunction(fs)
    }
  }
}

trait LowPriorityCanLift extends LowestPriorityCanLift {

  implicit def canVectorizeConst[T: ClassTag] = new CanVectorize[Const[T]] {
    type V = Const[DenseVector[T]]
    def lift(fs: Seq[Const[T]]): V = Const(DenseVector(fs.map(_.const): _*))
  }

  implicit def canVectorizeConstZero = new CanVectorize[ConstZero] {
    type V = ConstZero
    def lift(fs: Seq[ConstZero]): V = ConstZero()
  }

  implicit def canVectorizeConstOne = new CanVectorize[ConstOne] {
    type V = ConstOne
    def lift(fs: Seq[ConstOne]): V = ConstOne()
  }

  implicit def canVectorizeVar = new CanVectorize[Var] {
    type V = Var
    def lift(fs: Seq[Var]): V = Var()
  }

  implicit def canVectorizeHNil = new CanVectorize[HNil] {
    type V = HNil
    def lift(fs: Seq[HNil]): V = HNil
  }

  implicit def canVectorizeHList[F, L <: HList, T <: HList](
    implicit canVectorizeHead: CanVectorize[F],
    canVectorizeTail: CanVectorize.Aux[L, T]
  ) = new CanVectorize[F :: L] {
    type V = canVectorizeHead.V :: T
    def lift(fs: Seq[F :: L]): V = canVectorizeHead.lift(fs.map(_.head)) :: canVectorizeTail.lift(fs.map(_.tail))
  }

  implicit def canVectorizeProduct[L <: HList, U <: AnyRef { type V <: HList }](
    implicit canVectorizeHListSingleton: CanVectorize.SingletonOf[CanVectorize[L], U],
    liftedListAllSymbolicFunction: LUBConstraint[U#V, SymbolicFunction[_]]
  ) = new CanVectorize[Product[L]] {
    type V = Product[U#V]
    def lift(fs: Seq[Product[L]]): V = Product(canVectorizeHListSingleton.widen.lift(fs.map(_.fns)))
  }

  implicit def canVectorizeSum[L <: HList, U <: AnyRef { type V <: HList }](
    implicit canVectorizeHListSingleton: CanVectorize.SingletonOf[CanVectorize[L], U],
    liftedListAllSymbolicFunction: LUBConstraint[U#V, SymbolicFunction[_]]
  ) = new CanVectorize[Sum[L]] {
    type V = Sum[U#V]
    def lift(fs: Seq[Sum[L]]): V = Sum(canVectorizeHListSingleton.widen.lift(fs.map(_.fns)))
  }

  implicit def canVectorizeDifference[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2], LF1 <: SymbolicFunction[LF1], LF2 <: SymbolicFunction[LF2]](
    implicit canVectorizeF1: CanVectorize.Aux[F1, LF1],
    canVectorizeF2: CanVectorize.Aux[F2, LF2]
  ) = new CanVectorize[Difference[F1, F2]] {
    type V = Difference[LF1, LF2]
    def lift(fs: Seq[Difference[F1, F2]]) = Difference(canVectorizeF1.lift(fs.map(_.fn1)), canVectorizeF2.lift(fs.map(_.fn2)))
  }
  implicit def canVectorizeDoublesToDenseVector =
    new CanVectorize[Double] {
      type V = DenseVector[Double]
      override def lift(fs: Seq[Double]): DenseVector[Double] =
        DenseVector(fs: _*)
    }
}

object CanVectorize extends LowPriorityCanLift {
  type Aux[E, V0] = CanVectorize[E] { type V = V0 }
  implicit class RichLiftable[E](fs: Seq[E]) {
    def lifted(implicit canVectorize: CanVectorize[E]): canVectorize.V = canVectorize.lift(fs)
  }
  case class SingletonOf[T, U <: { type V }](widen: T { type V = U#V })
  object SingletonOf {
    implicit def mkSingleton[T <: {type V}](implicit t: T) =
      SingletonOf[T, t.type](t)
  }


  implicit def canVectorizeLabelledHList[K, V0, L <: HList, TL <: HList, KL <: HList, LL <: HList](
    implicit canVectorizeHead: CanVectorize[V0],
    canVectorizeTail: CanVectorize.Aux[L, TL],
    keys: Keys.Aux[FieldType[K, V0] :: L, KL],
    isHCons: IsHCons[KL]
  ) = new CanVectorize[FieldType[K, V0] :: L] {
    type V = FieldType[isHCons.H, canVectorizeHead.V] :: TL
    def lift(fs: Seq[FieldType[K, V0] :: L]): V = {
      val headKey: isHCons.H = keys().head.asInstanceOf[isHCons.H]
      val headValue: canVectorizeHead.V = canVectorizeHead.lift(fs.map(_.head))
      (headValue.asInstanceOf[FieldType[isHCons.H, canVectorizeHead.V]]) :: canVectorizeTail.lift(fs.map(_.tail))
    }
  }

}

object vectorize extends UFunc with MappingUFunc {
  implicit def implCanVectorize[F](implicit canVectorize: CanVectorize[F]): Impl[Seq[F], canVectorize.V] = new Impl[Seq[F], canVectorize.V] {
    def apply(f: Seq[F]) = canVectorize.lift(f)
  }
}