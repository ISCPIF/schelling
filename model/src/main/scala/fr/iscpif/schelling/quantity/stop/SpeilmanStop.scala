/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.iscpif.schelling.quantity.stop

import fr.iscpif.schelling.quantity._

import scala.util.{ Random, Try }

trait SpeilmanStop <: Schelling {

  def maxStep = 200
  def maxUnsatisfied = 0.98
  def unsatisfiedWindow = 20
  def minimumVariation = 0.1

  def run(implicit rng: Random): Option[State] = {
    def average(s: Seq[Double]) = s.sum / s.size
    val windows = states.map { s => s -> average(unsatisfieds(s).map(_.number.toDouble)) }.zipWithIndex.sliding(unsatisfiedWindow)

    def lastState(window: Seq[((State, Double), Int)]): Option[State] = {
      def tooManySteps = window.last._2 >= maxStep
      def maxUnsatisfiedReached = window.exists { case ((_, u), _) => u > maxUnsatisfied }
      def underMinimumVariation = (window.unzip._2.max - window.unzip._2.min) < minimumVariation
      if (tooManySteps) None
      else if (maxUnsatisfiedReached || underMinimumVariation) Some(window.last._1._1)
      else lastState(windows.next)
    }

    lastState(windows.next)
  }
}
