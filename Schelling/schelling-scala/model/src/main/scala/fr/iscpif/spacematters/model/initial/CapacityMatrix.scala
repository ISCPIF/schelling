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

import fr.iscpif.spacematters.model.Cell

import scala.util.Random

trait CapacityMatrix {
  def capacityGrid(implicit rng: Random): Seq[Seq[Int]]

  def capacityMatrix(implicit rng: Random) =
    capacityGrid.map(_.map(c ⇒ Cell(capacity = c, green = 0, red = 0)))
}
