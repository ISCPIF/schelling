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
import fr.iscpif.spacematters.model.{Position, State}

trait Neighborhood {

  def neighborhoodSize: Int

  def neighbors(state: State, position: Position) =
    for {
      oi <- -neighborhoodSize to neighborhoodSize
      oj <- -neighborhoodSize to neighborhoodSize
      (i, j) = position
    } yield state(i + oi)(j + oj)

}
