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
package fr.iscpif.spacematters.model.move

import fr.iscpif.schelling.quantity._
import fr.iscpif.spacematters.model.{Position, State, Color}

import scala.util.Random

case class Move(color: Color, origin: Position, destination: Position)

trait Moves <: Unsatisfieds {
  def moves(state: State)(implicit rng: Random): Seq[Move] = {
    for {
      unsatisfied <- unsatisfieds(state)
      i <- 0 until unsatisfied.number
    } yield Move(unsatisfied.color, unsatisfied.position, destination(state, unsatisfied))
  }

  def destination(state: State, unsatisfied: Unsatisfied)(implicit rng: Random): Position
}

trait RandomMoves <: Moves {

  def destination(state: State, unsatisfied: Unsatisfied)(implicit rng: Random) = {
    val x = rng.nextInt(state.side)
    val y = rng.nextInt(state.side)
    (x, y)
  }
}
