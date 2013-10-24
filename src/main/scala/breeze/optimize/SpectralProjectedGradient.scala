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
class SpectralProjectedGradient[T, -DF <: StochasticDiffFunction[T]](
  val projection: T => T = { (t: T) => t },
  tolerance: Double = 1e-6,
  val suffDec: Double = 1e-4,
  minImprovementWindow: Int = 10,
  val alphaMax: Double = 1e10,
  val alphaMin: Double = 1e-10,
  maxIter: Int = 500,
  val testOpt: Boolean = true,
  val initFeas: Boolean = false,
  val maxSrchIt: Int = 30)(implicit coord: MutableCoordinateSpace[T, Double]) extends FirstOrderMinimizer[T, DF](minImprovementWindow = minImprovementWindow, maxIter = maxIter, tolerance = tolerance) with Logging {
  import coord._
  type History = Double
  def correctedVector(x: T, g: T): T = projection(x + g) - x
  protected def initialHistory(f: DF, init: T): History = 1.0
  protected def chooseDescentDirection(state: State, f: DF): T = ???
  protected def takeStep(state: State, dir: T, stepSize: Double): T = ???
  protected def updateHistory(newX: T, newGrad: T, newVal: Double, f: DF, oldState: State): History = {
    val y = newGrad - oldState.grad
    val s = newX - oldState.x
    val alpha = s.dot(s) / s.dot(y)
    if (alpha < alphaMin || alpha > alphaMax)
      1
    else
      alpha
  }

  override def minimize(f: DF, init: T): T = {
    var gnorm: Double = 0.0
    var x = if (initFeas) copy(init) else projection(copy(init))

    var alpha: Double = 1.0 //0.001 / gnorm
    var iterationsExhausted = false
    var grad = f.gradientAt(x)
    var value = f.valueAt(x)
    var funEvals = 1
    var currentState = initialState(f, init)
    breakable {
      while (funEvals < maxIter) {
        val oldState = currentState
        val fmax = updateFValWindow(currentState, value)
        val d = correctedVector(x, grad * -oldState.history)
        val gtd = grad.dot(d)
        if (gtd > -tolerance)
          break;
        var t = if (oldState.iter == 1) {
          scala.math.min(1.0, (1.0 / norm(grad, 1)))
        } else {
          1.0
        }
        if (oldState.iter > 0) {
          alpha = updateHistory(x, grad, value, f, currentState)
        }
        currentState = State(x, value, grad, value, grad,oldState.iter + 1, value, alpha, fmax, 0, false)
        // Backtracking line-search
        val res = determineStepSize(currentState, f, d)
        x = x + d * res
        value = f(x)
        grad = f.gradientAt(x)
        funEvals += 5
        var optCond = norm(correctedVector(x, -grad), 1)
        if (optCond < tolerance)
          break
        if (norm(d * t, 1) < tolerance)
          break
        if (scala.math.abs(value - currentState.value) < tolerance)
          break
      }
    }

    x
  }
  protected def determineStepSize(state: State, f: DF, direction: T): Double = {
    import state._
    val funRef = fVals.max
    var lineSearchIters = 0
    var t = history
    var xNew = x + direction * t
    var fNew = f(xNew)
    var gNew = f.gradientAt(xNew)
    var searchStep = xNew - x
    var sufficientDecrease = grad.dot(searchStep) * suffDec
    var funEvals = 1
    breakable {
      while (fNew > funRef + sufficientDecrease) {
        var temp = t
        t = t / 2
        if (norm(direction * t, 1) < tolerance || t == 0) {
          t = 0
          break
        }
        xNew = x + direction * t
        fNew = f(xNew)
        gNew = f.gradientAt(xNew)
        funEvals += 1
        lineSearchIters += 1
      }
    }
    t
  }

}
