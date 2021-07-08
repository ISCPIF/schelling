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

import fr.iscpif.spacematters.model.Schelling

import scala.util.Random

trait RandomCapacityMatrix <: CapacityMatrix { self: Schelling ⇒

  def maxCapacity: Int
  def size: Int

  override def capacityGrid(implicit rng: Random): Seq[Seq[Int]] =
    Seq.fill(size, size) { rng.nextInt(maxCapacity) }

}
