package com.ankurdave.ttt

trait Factor {
  def updateMessage(index: Int): Double
}

case class PriorFactor(mu: Double, sigma2: Double, variable: Gaussian) extends Factor {
  private val message = Gaussian(0.0, 0.0)

  override def updateMessage(index: Int): Double = {
    assert(index == 0)
    val oldMarginal = variable.copy()

    val newMessage = Gaussian.fromMS2(mu, sigma2)
    val newMarginal = Gaussian(
      variable.precisionMean + newMessage.precisionMean - message.precisionMean,
      variable.precision + newMessage.precision - message.precision)

    variable.set(newMarginal)
    message.set(newMessage)

    oldMarginal - newMarginal
  }
}

/** N(variable1;variable2,betaSquared) */
case class LikelihoodFactor(variable1: Gaussian, variable2: Gaussian, betaSquared: Double)
  extends Factor {

  private val prec = 1.0 / betaSquared

  private val message1 = Gaussian(0.0, 0.0)
  private val message2 = Gaussian(0.0, 0.0)

  private def updateHelper(m1: Gaussian, m2: Gaussian, v1: Gaussian, v2: Gaussian): Double = {
    val oldMarginal = v1.copy()

    val a = prec / (prec + v2.precision - m2.precision)
    val newMessage = Gaussian(
      a * (v2.precisionMean - m2.precisionMean),
      a * (v2.precision - m2.precision))
    val oldMarginalWithoutMsg = v1 / m1
    val newMarginal = oldMarginalWithoutMsg * newMessage

    m1.set(newMessage)
    v1.set(newMarginal)

    newMarginal - oldMarginal
  }

  override def updateMessage(index: Int): Double = index match {
    case 0 => updateHelper(message1, message2, variable1, variable2)
    case 1 => updateHelper(message2, message1, variable2, variable1)
  }
}

/** variable0 = a1 * variable1 + a2 * variable2 */
case class WeightedSumFactor(
    variable0: Gaussian, variable1: Gaussian, variable2: Gaussian,
    a1: Double, a2: Double)
  extends Factor {

  private val message1 = Gaussian(0.0, 0.0)
  private val message2 = Gaussian(0.0, 0.0)
  private val message3 = Gaussian(0.0, 0.0)

  private val w0_0 = a1
  private val w0_1 = a2

  private val w1_0 = -a2 / a1
  private val w1_1 = 1.0 / a1

  private val w2_0 = -a1 / a2
  private val w2_1 = 1.0 / a2

  private val w0s_0 = w0_0 * w0_0
  private val w0s_1 = w0_1 * w0_1

  private val w1s_0 = w1_0 * w1_0
  private val w1s_1 = w1_1 * w1_1

  private val w2s_0 = w2_0 * w2_0
  private val w2s_1 = w2_1 * w2_1

  private def updateHelper(
    w_0: Double, w_1: Double, ws_0: Double, ws_1: Double,
    m1: Gaussian, m2: Gaussian, m3: Gaussian,
    v1: Gaussian, v2: Gaussian, v3: Gaussian): Double = {
    val oldMarginal = v1.copy()

    val d0 = v2 / m2
    val d1 = v3 / m3
    val denom = ws_0 * d1.precision + ws_1 * d0.precision
    val newPrecision = d0.precision * d1.precision / denom
    val newPrecisionMean =
      (w_0 * d1.precision * d0.precisionMean + w_1 * d0.precision * d1.precisionMean) / denom
    val newMessage = Gaussian(newPrecisionMean, newPrecision)
    val oldMarginalWithoutMsg = v1 / m1
    val newMarginal = oldMarginalWithoutMsg * newMessage

    m1.set(newMessage)
    v1.set(newMarginal)

    newMarginal - oldMarginal
  }

  override def updateMessage(index: Int): Double = index match {
    case 0 => updateHelper(
      w0_0, w0_1, w0s_0, w0s_1,
      message1, message2, message3,
      variable0, variable1, variable2)
    case 1 => updateHelper(
      w1_0, w1_1, w1s_0, w1s_1,
      message2, message3, message1,
      variable1, variable2, variable0)
    case 2 => updateHelper(
      w2_0, w2_1, w2s_0, w2s_1,
      message3, message2, message1,
      variable2, variable1, variable0)
  }
}

/** variable > 0 */
case class GreaterThanZeroFactor(variable: Gaussian) extends Factor {
  private val message = Gaussian(0.0, 0.0)

  override def updateMessage(index: Int): Double = {
    assert(index == 0)
    val oldMarginal = variable.copy()
    val msgFromVar = variable / message
    val c = msgFromVar.precision
    val d = msgFromVar.precisionMean
    val sqrtC = math.sqrt(c)
    val dOnSqrtC = d / sqrtC
    val denom = 1.0 - Statistics.w(dOnSqrtC)
    val newPrecision = c / denom
    val newPrecisionMean = (d + sqrtC * Statistics.v(dOnSqrtC)) / denom
    val newMarginal = Gaussian(newPrecisionMean, newPrecision)
    val newMessage = message * newMarginal / variable

    message.set(newMessage)
    variable.set(newMarginal)

    newMarginal - oldMarginal
  }
}
