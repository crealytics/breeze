package breeze.symbolic

import shapeless.LUBConstraint.<<:
import shapeless._

import scala.util.matching.Regex

/**
  * Base trait for all symbolic functions.
  * Not sealed because we want to allow users to
  * create their own subclasses.
  */
trait SymbolicFunction

case class Var() extends SymbolicFunction {
  override def toString = "Var"
}

case class Identity[T]() extends SymbolicFunction {
  override def toString = "Identity"
}

trait SingleArgumentFunction[F <: SymbolicFunction] extends SymbolicFunction {
  def fn: F
}

case class Exponential[F <: SymbolicFunction](fn: F) extends SingleArgumentFunction[F] {
  override def toString = s"exp($fn)"
}

case class Logarithm[F <: SymbolicFunction](fn: F) extends SingleArgumentFunction[F] {
  override def toString = s"log($fn)"
}


trait MultipleArgumentFunction[L <: HList] extends SymbolicFunction {
  def fns: L

  def operatorSymbol: String

  override def toString =
    s"($fns)"
      .replaceAll(" :: ", s" $operatorSymbol ")
      .replaceAll(s" ${Regex.quote(operatorSymbol)} HNil", "")
}

case class Sum[L <: HList : <<:[SymbolicFunction]#λ](fns: L) extends MultipleArgumentFunction[L] {
  override def operatorSymbol = "+"
}

case class Product[L <: HList : <<:[SymbolicFunction]#λ](fns: L) extends MultipleArgumentFunction[L] {
  override def operatorSymbol = "*"
}

trait TwoArgumentFunction[+F1 <: SymbolicFunction, +F2 <: SymbolicFunction] extends SymbolicFunction {
  def fn1: F1

  def fn2: F2
}



case class Division[F1 <: SymbolicFunction, F2 <: SymbolicFunction](fn1: F1, fn2: F2)
  extends TwoArgumentFunction[F1, F2] {
  override def toString = s"$fn1 / $fn2"
}


case class Difference[F1 <: SymbolicFunction, F2 <: SymbolicFunction](fn1: F1, fn2: F2)
  extends TwoArgumentFunction[F1, F2] {
  override def toString = s"($fn1 - $fn2)"
}


trait ConstBase extends SymbolicFunction

case class Const[T](const: T) extends ConstBase {
  override def toString = const.toString
}

/*
  * Having special ConstZero and ConstOne classes
  * allows us to retain compile-time information on zeroness
  * which can be used to e.g. simplify functions.
  */
case class ConstZero() extends ConstBase {
  override def toString = "0.00"
}

case class ConstOne() extends ConstBase {
  override def toString = "1.00"
}

case class Chain[H <: SymbolicFunction, G <: SymbolicFunction](firstFunc: H, secondFunc: G) extends SymbolicFunction {
  override def toString = s"$firstFunc($secondFunc)"
}

case class Inversion[F <: SymbolicFunction](f: F) extends SymbolicFunction {
  override def toString = s"Inverse($f)"
}

case class VectorizedSymbolicFunction[F <: SymbolicFunction](fs: Seq[F]) extends SymbolicFunction
