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
package fr.iscpif.spacematters.model.container

import fr.iscpif.spacematters.model.Cell

import scala.util.Random

trait Container {

  def size: Int

  def container(implicit rng: Random): Seq[Seq[Cell]]

  /**
   * Stringify just to check validity of generators ; has no sense in general context as a new instance will be randomly generated at each
   * call of container.
   *
   * @return
   */
  override def toString: String = {
    var res = ""
    container(new Random).foreach(
      (row: Seq[Cell]) => {
        row.foreach(
          (c: Cell) => { res = res + c.capacity + " | " }
        )
        res = res + "\n"
      }
    )
    res
  }

}
