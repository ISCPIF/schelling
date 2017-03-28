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

trait Empty[T] {
  def empty: T
}

object Empty {
  implicit val emptyCell =
    new Empty[Cell] {
      def empty = Cell.empty
    }

  implicit val emptyInt =
    new Empty[Int] {
      def empty = 0
    }
}

case class Matrix[T](matrix: Seq[Seq[T]]) {
  def numberOfCells = side * side
  def side = matrix.size

  def apply(i: Int)(j: Int)(implicit empty: Empty[T]): T =
    if (i < 0 || j < 0 || i >= side || j >= side) empty.empty
    else matrix(i)(j)

  def cells = matrix.flatZipWithIndex

  def zipWithPosition: Seq[(T, Position)] =
    for {
      (row, i) ← matrix.zipWithIndex
      (content, j) ← row.zipWithIndex
    } yield content -> (i, j)
}