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
package fr.iscpif.spacematters

import scala.util.Random

package object model {

  implicit class SeqDecorator[T](s: Seq[T]) {
    def random(implicit rng: Random) = s((s.size * rng.nextDouble).toInt)
  }

  implicit class SeqOfSeqDecorator[T](s: Seq[Seq[T]]) {
    def flatZipWithIndex =
      for {
        (l, i) ← s.zipWithIndex
        (e, j) ← l.zipWithIndex
      } yield (i, j) -> e
  }

  type State = Matrix[Cell]
  type Position = (Int, Int)
  type Quantity[T] = T ⇒ Double

  implicit def colorToCellQuantity(c: Color): Quantity[Cell] = c.cellColor.get(_)
  implicit def matrixToSeqOfSeq[T](m: Matrix[T]) = m.matrix
}
