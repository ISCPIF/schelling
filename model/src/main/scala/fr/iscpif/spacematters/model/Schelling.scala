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

import fr.iscpif.schelling.quantity.initial._
import fr.iscpif.schelling.quantity.move._
import fr.iscpif.spacematters.model.initial.InitialState
import fr.iscpif.spacematters.model.move.Moves

import scala.util.Random

trait Schelling <: InitialState with Moves {

  def size: Int
  def greenRatio: Double
  def redRatio: Double
  def neighborhoodSize = 2

  def step(state: State)(implicit rng: Random) = {

    val builder = state.matrix.map(_.toBuffer).toBuffer

    for {
      Move(color, origin, destination) <- rng.shuffle(moves(state))
      destinationCell = builder(destination._1)(destination._2)
      if !destinationCell.isFull
      originCell = builder(origin._1)(origin._2)
    } {
      builder(origin._1)(origin._2) = color.cellColor.modify(_ - 1)(originCell)
      builder(destination._1)(destination._2) = color.cellColor.modify(_ + 1)(destinationCell)
    }

    State(builder)
  }

  def states(implicit rng: Random) = Iterator.iterate(initialState)(step)

}