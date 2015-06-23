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
package fr.iscpif.spacematters.model.stop

import fr.iscpif.schelling.quantity._
import fr.iscpif.spacematters.model.{Schelling, State}

import scala.util.{ Random, Try }

trait SpeilmanStop <: Schelling {

  case class ResultState(step: Int, state: State)

  def maxStep = 200
  def maxSatisfied = 0.98
  def satisfiedWindow = 20
  def minimumVariation = 0.1

  def run(implicit rng: Random): ResultState = {
    def average(s: Seq[Double]) = s.sum / s.size
    val windows = states.map { s => s -> unsatisifedRatio(s) }.zipWithIndex.sliding(satisfiedWindow)

    def lastState(window: Seq[((State, Double), Int)]): ResultState = {
      def tooManySteps = window.last._2 >= maxStep
      def maxUnsatisfiedReached = window.exists { case ((_, u), _) => 1 - u > maxSatisfied }

      def unsatisfieds = window.map { case ((_, u), _) => u }
      def underMinimumVariation = (unsatisfieds.max - unsatisfieds.min) < minimumVariation

      if (tooManySteps || maxUnsatisfiedReached || underMinimumVariation) ResultState(window.last._2, window.last._1._1)
      else lastState(windows.next)
    }

    lastState(windows.next)
  }
}
