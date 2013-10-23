package breeze.optimize

import breeze.math.MutableCoordinateSpace
import com.typesafe.scalalogging.log4j.Logging
import breeze.collection.mutable.RingBuffer
import scala.util.control.Breaks._

/**
 * SPG is a Spectral Projected Gradient minimizer; it minimizes a differentiable
 * function subject to the optimum being in some set, given by the projection operator  projection
 * @tparam T vector type
 * @param optTol termination criterion: tolerance for norm of projected gradient
 * @param gamma  sufficient decrease parameter
 * @param M number of history entries for linesearch
 * @param alphaMax longest step
 * @param alphaMin shortest step
 * @param maxNumIt maximum number of iterations
 * @param testOpt perform optimality check based on projected gradient at each iteration
 * @param initFeas is the initial guess feasible, or should it be projected?
 * @param maxSrchIt maximum number of line search attempts
 * @param projection projection operations
 */
class SpectralProjectedGradient[T](
  val projection: T => T = { (t: T) => t },
  val optTol: Double = 1e-6,
  val suffDec: Double = 1e-4,
  val M: Int = 10,
  val alphaMax: Double = 1e10,
  val alphaMin: Double = 1e-10,
  val maxNumIt: Int = 500,
  val testOpt: Boolean = true,
  val initFeas: Boolean = false,
  val maxSrchIt: Int = 30)(implicit coord: MutableCoordinateSpace[T, Double]) extends Minimizer[T, DiffFunction[T]] with Logging {
  import coord._
  override def minimize(prob: DiffFunction[T], guess: T): T = {
    def correctedGradient(x: T, g: T): T = projection(x + g) - x
    var gnorm: Double = 0.0
    var x = if (initFeas) copy(guess) else projection(copy(guess))

    var alpha: Double = 1.0 //0.001 / gnorm
    var fmax = new RingBuffer[Double](M)
    var i = 1
    var iterationsExhausted = false
    var g = prob.gradientAt(x)
    var f = prob.valueAt(x)
    var xOld: T = x
    var gOld: T = g
    var fOld: Double = Double.PositiveInfinity
    var funEvals = 1
    breakable {
      while (funEvals < maxNumIt) {
        if (i == 1) {
          alpha = 1.0
        } else {
          val y = g - gOld
          val s = x - xOld
          alpha = s.dot(s) / s.dot(y)
          if (alpha < alphaMin || alpha > alphaMax)
            alpha = 1
        }
        var d = g * -alpha
        fOld = f
        xOld = x
        gOld = g
        d = projection(x + d) - x
        val gtd = g.dot(d)
        if (gtd > -optTol)
          break;
        var t = if (i == 1) {
          scala.math.min(1.0, (1.0 / norm(g, 1)))
        } else {
          1.0
        }
        fmax += f
        // Backtracking line-search
        val funRef = fmax.max
        val res = nonMonotoneLineSearch(x, prob, d, g, funRef, f, t)
        x = res._1
        f = res._2
        g = res._3
        funEvals += res._5
        var optCond = norm(projection(x - g) - x, 1)
        if(optCond < optTol)
          break
        if(norm(d*t,1) < optTol)
          break
        if(scala.math.abs(f - fOld) < optTol)
          break
        i = i + 1
      }
    }

    x
  }

  def nonMonotoneLineSearch(x: T, prob: DiffFunction[T], d: T, g: T, funRef: Double, f: Double, initialAlpha: Double = 1.0) = {
    var lineSearchIters = 0
    var t = initialAlpha
    var xNew = x + d * t
    var fNew = prob(xNew)
    var gNew = prob.gradientAt(xNew)
    var searchStep = xNew - x
    var sufficientDecrease = g.dot(searchStep) * suffDec
    var funEvals = 1
    breakable {
      while (fNew > funRef + sufficientDecrease) {
        var temp = t
        t = t / 2
        if (norm(d * t, 1) < optTol || t == 0) {
          t = 0
          fNew = f
          gNew = g
          break
        }
        xNew = x + d * t
        fNew = prob(xNew)
        gNew = prob.gradientAt(xNew)
        funEvals += 1
        lineSearchIters += 1
      }
    }
    (xNew, fNew, gNew, t, funEvals)
  }

}
