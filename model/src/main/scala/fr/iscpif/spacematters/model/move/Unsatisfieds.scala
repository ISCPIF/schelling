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

case class Unsatisfied(position: Position, color: Color, number: Int)

trait Unsatisfieds <: Neighborhood {

  def similarWanted: Double

  def unsatisfied(state: State, position: Position, color: Color) = {
    val neighbourCells = neighbors(state, position)
    val population = neighbourCells.map(_.population).sum
    val colorPopulation = neighbourCells.map(color.cellColor.get).sum
    colorPopulation.toDouble / population < similarWanted
  }

  def unsatisfieds(state: State): Seq[Unsatisfied] = {
    for {
      (position, c) <- state.cells
      color <- Color.all
      if unsatisfied(state, position, color)
    } yield Unsatisfied(position, color, color.cellColor.get(c))
  }

  def unsatisifedRatio(state: State) = {
    val numberOfUnsatisfieds = unsatisfieds(state).map(_.number).sum
    numberOfUnsatisfieds.toDouble / state.matrix.flatten.map(_.population).sum
  }
}
