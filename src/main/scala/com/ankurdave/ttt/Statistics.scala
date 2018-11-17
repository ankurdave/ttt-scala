package com.ankurdave.ttt

object Statistics {
  /** Complementary error function: 2/sqrt(pi) * integral from x to infinity of exp (-t^2) dt */
  def erfc(x: Double): Double = {
    if (x == Double.NegativeInfinity) 2.0
    else if (x == Double.PositiveInfinity) 0.0
    else {
      val z = math.abs(x)
      val t = 1.0 / (1.0 + 0.5 * z)
      val res = t * math.exp(
        -z * z - 1.26551223 + t * (
          1.00002368 + t * (
            0.37409196 + t * (
              0.09678418 + t * (
                -0.18628806 + t * (
                  0.27886807 + t * (
                    -1.13520398 + t * (
                      1.48851587 + t * (
                        -0.82215223 + t *
                          0.17087277)))))))))
      if (x >= 0.0) res else 2.0 - res
    }
  }

  /** Cumulative Gaussian distribution */
  def normcdf(t: Double): Double = {
    val sqrt2 = 1.4142135623730951
    return (erfc(-t / sqrt2)) / 2.0
  }

  /** Gaussian density */
  def normpdf(t: Double): Double = {
    val invsqrt2pi = 0.398942280401433
    return invsqrt2pi * math.exp(-(t * t / 2.0))
  }

  /** Additive correction of a single-sided truncated Gaussian with unit variance */
  def v(t: Double): Double = {
    val denom = normcdf(t)
    if (denom < 2.222758749e-162) -t
    else normpdf(t) / denom
  }

  /** Multiplicative correction of a single-sided truncated Gaussian with unit variance */
  def w(t: Double): Double = {
    val denom = normcdf(t)
    if (denom < 2.222758749e-162) {
      if (t < 0.0) 1.0 else 0.0
    } else {
      val vt = v(t)
      vt * (vt + t)
    }
  }
}
