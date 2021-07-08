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
package fr.iscpif.spacematters.model

import fr.iscpif.spacematters.model.initial._
import fr.iscpif.spacematters.model.move._

import scala.util.Random

trait Schelling <: InitialState with Moves {

  def greenRatio: Double
  def redRatio: Double
  def neighborhoodSize = 2

  def step(state: State)(implicit rng: Random) = {

    val builder = state.matrix.map(_.toArray).toArray

    for {
      Move(color, origin, destination) ← rng.shuffle(moves(state))
      if (origin != destination)
    } {
      val destinationCell = builder(destination._1)(destination._2)
      val originCell = builder(origin._1)(origin._2)
      if (!destinationCell.isFull) {
        builder(origin._1)(origin._2) = color.cellColor.modify(_ - 1)(originCell)
        builder(destination._1)(destination._2) = color.cellColor.modify(_ + 1)(destinationCell)
      }
    }

    Matrix(builder.map(_.toSeq).toSeq)
  }

  def states(implicit rng: Random) = Iterator.iterate(initialState)(step)

}
