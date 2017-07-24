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
package fr.iscpif.spacematters.model.initial

import fr.iscpif.spacematters.model._
import fr.iscpif.spacematters.model.container._

import scala.util.Random

trait RandomState <: InitialState with Container { self: Schelling =>

  def maxCapacity: Int

  def initialState(implicit rng: Random) = {

    val cells = container(rng)
    val totalCapacity = cells.flatten.map(_.capacity).sum
    val greens = (totalCapacity * greenRatio).toInt
    val reds = (totalCapacity * redRatio).toInt

    fill(cells, Cell(totalCapacity, reds, greens))
  }

  def fill(cells: Seq[Seq[Cell]], reservoir: Cell)(implicit rng: Random) = {
    val builder = cells.map(_.toBuffer).toBuffer

    def fill(reservoir: Cell): Unit =
      if (!reservoir.isEmpty) {
        def color = if (rng.nextDouble() < reservoir.green.toDouble / reservoir.population) Green else Red
        val i = (rng.nextDouble() * cells.size).toInt
        val j = (rng.nextDouble() * cells.size).toInt
        if (!builder(i)(j).isFull) {
          builder(i)(j) = color.cellColor.modify(_ + 1)(builder(i)(j))
          fill(color.cellColor.modify(_ - 1)(reservoir))
        } else fill(reservoir)
      }

    fill(reservoir)
    Matrix(builder)
  }
}
