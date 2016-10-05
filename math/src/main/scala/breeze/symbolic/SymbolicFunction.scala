package breeze.symbolic

import shapeless.LUBConstraint.<<:
import shapeless._

import scala.util.matching.Regex
import breeze.linalg._
import breeze.linalg.operators.{OpAdd, OpMulScalar}
import breeze.numerics._
import shapeless.ops.hlist.Prepend

/**
  * Base trait for all symbolic functions.
  * Not sealed because we want to allow users to
  * create their own subclasses.
  */
trait SymbolicFunction[+F <: SymbolicFunction[F]] extends NumericOps[F]

case class Var() extends SymbolicFunction[Var] {
  override def toString = "Var"
  override def repr: Var = this
}

case class Identity[T]() extends SymbolicFunction[Identity[T]] {
  override def toString = "Identity"
  override def repr: Identity[T] = this
}

trait SingleArgumentFunction[F <: SymbolicFunction[F], +This <: SingleArgumentFunction[F, This]] extends SymbolicFunction[This] {
  def fn: F
}

case class Exponential[F <: SymbolicFunction[F]](fn: F) extends SingleArgumentFunction[F, Exponential[F]] {
  override def toString = s"exp($fn)"
  override def repr: Exponential[F] = this
}

case class Logarithm[F <: SymbolicFunction[F]](fn: F) extends SingleArgumentFunction[F, Logarithm[F]] {
  override def toString = s"log($fn)"
  override def repr: Logarithm[F] = this
}


trait MultipleArgumentFunction[L <: HList, +This <: MultipleArgumentFunction[L, This]] extends SymbolicFunction[This] {
  def fns: L

  def operatorSymbol: String

  override def toString =
    s"($fns)"
      .replaceAll(" :: ", s" $operatorSymbol ")
      .replaceAll(s" ${Regex.quote(operatorSymbol)} HNil", "")
}

case class Sum[L <: HList : <<:[SymbolicFunction[_]]#λ](fns: L) extends MultipleArgumentFunction[L, Sum[L]] {
  override def operatorSymbol = "+"
  override def repr: Sum[L] = this
}

case class Product[L <: HList : <<:[SymbolicFunction[_]]#λ](fns: L) extends MultipleArgumentFunction[L, Product[L]] {
  override def operatorSymbol = "*"
  override def repr: Product[L] = this
}

trait TwoArgumentFunction[+F1 <: SymbolicFunction[_], +F2 <: SymbolicFunction[_], +This <: TwoArgumentFunction[F1, F2, This]] extends SymbolicFunction[This] {
  def fn1: F1
  def fn2: F2
}

case class Division[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]](fn1: F1, fn2: F2)
  extends TwoArgumentFunction[F1, F2, Division[F1, F2]] {
  override def toString = s"$fn1 / $fn2"
  override def repr: Division[F1, F2] = this
}


case class Difference[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]](fn1: F1, fn2: F2)
  extends TwoArgumentFunction[F1, F2, Difference[F1, F2]] {
  override def toString = s"($fn1 - $fn2)"
  override def repr: Difference[F1, F2] = this
}


trait ConstBase[+This <: ConstBase[This]] extends SymbolicFunction[This]

case class Const[T](const: T) extends ConstBase[Const[T]] {
  override def toString = const.toString
  override def repr: Const[T] = this
}

/*
  * Having special ConstZero and ConstOne classes
  * allows us to retain compile-time information on zeroness
  * which can be used to e.g. simplify functions.
  */
case class ConstZero() extends ConstBase[ConstZero] {
  override def toString = "0.00"
  override def repr: ConstZero = this
}

case class ConstOne() extends ConstBase[ConstOne] {
  override def toString = "1.00"
  override def repr: ConstOne = this
}

case class Chain[H <: SymbolicFunction[H], G <: SymbolicFunction[G]](firstFunc: H, secondFunc: G) extends SymbolicFunction[Chain[H, G]] {
  override def toString = s"$firstFunc($secondFunc)"
  override def repr: Chain[H, G] = this
}

case class Inversion[F <: SymbolicFunction[F]](f: F) extends SymbolicFunction[Inversion[F]] {
  override def toString = s"Inverse($f)"
  override def repr: Inversion[F] = this
}

case class VectorizedSymbolicFunction[F <: SymbolicFunction[F]](fs: Seq[F]) extends SymbolicFunction[VectorizedSymbolicFunction[F]] {
  override def repr: VectorizedSymbolicFunction[F] = this
}

trait LowPrioritySymbolicFunctionImplicits {
  implicit def addSymbolicFunctions[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]]: OpAdd.Impl2[F1, F2, Sum[F1 :: F2 :: HNil]] = new OpAdd.Impl2[F1, F2, Sum[F1 :: F2 :: HNil]] {
    override def apply(f1: F1, f2: F2): Sum[::[F1, ::[F2, HNil]]] = {
      Sum(f1 :: f2 :: HNil)
    }
  }

  implicit def multiplySymbolicFunctions[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]]: OpMulScalar.Impl2[F1, F2, Product[F1 :: F2 :: HNil]] = new OpMulScalar.Impl2[F1, F2, Product[F1 :: F2 :: HNil]] {
    override def apply(f1: F1, f2: F2): Product[::[F1, ::[F2, HNil]]] = {
      Product(f1 :: f2 :: HNil)
    }
  }
}

object SymbolicFunction extends LowPrioritySymbolicFunctionImplicits {
  implicit def logSymbolicFunction[F <: SymbolicFunction[F]]: log.Impl[F, Logarithm[F]] = new log.Impl[F, Logarithm[F]] {
    def apply(f: F) = Logarithm(f)
  }

  implicit def expSymbolicFunction[F <: SymbolicFunction[F]]: exp.Impl[F, Exponential[F]] = new exp.Impl[F, Exponential[F]] {
    def apply(f: F) = Exponential(f)
  }

  implicit def sumOfSymbolicFunctionSums[L <: HList : <<:[SymbolicFunction[_]]#λ, F <: SymbolicFunction[F], Out <: HList](
    implicit prepend: Prepend.Aux[L, F :: HNil, Out],
    ev: LUBConstraint[Out, SymbolicFunction[_]]
  ): OpAdd.Impl2[Sum[L], F, Sum[Out]] =
    new OpAdd.Impl2[Sum[L], F, Sum[Out]] {
    override def apply(l: Sum[L], f: F): Sum[Out] = {
      Sum(l.fns ::: f :: HNil)
    }
  }

  implicit def productOfSymbolicFunctionProducts[L <: HList : <<:[SymbolicFunction[_]]#λ, F <: SymbolicFunction[F], Out <: HList](
    implicit prepend: Prepend.Aux[L, F :: HNil, Out],
    ev: LUBConstraint[Out, SymbolicFunction[_]]
  ): OpMulScalar.Impl2[Product[L], F, Product[Out]] =
    new OpMulScalar.Impl2[Product[L], F, Product[Out]] {
      override def apply(l: Product[L], f: F): Product[Out] = {
        Product(l.fns ::: f :: HNil)
      }
    }
}
