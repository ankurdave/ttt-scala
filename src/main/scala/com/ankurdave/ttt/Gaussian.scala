package com.ankurdave.ttt

case class Gaussian(var precisionMean: Double, var precision: Double) {
  def pi: Double = precisionMean
  def tau: Double = precision
  def mean: Double = precisionMean / precision
  def mu: Double = mean
  def variance: Double = 1.0 / precision
  def sigma: Double = math.sqrt(variance)

  def *(b: Gaussian): Gaussian =
    Gaussian(pi + b.pi, tau + b.tau)
  def /(b: Gaussian): Gaussian =
    Gaussian(pi - b.pi, tau - b.tau)
  /** Absolute difference */
  def -(b: Gaussian): Double =
    math.max(math.abs(pi - b.pi), math.sqrt(math.abs(tau - b.tau)))

  def set(b: Gaussian): Unit = {
    precisionMean = b.precisionMean
    precision = b.precision
  }
}
