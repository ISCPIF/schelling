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
package fr.iscpif.spacematters.model.metric

import fr.iscpif.spacematters.model._

object Moran {

  def localNeighbourhoodPairs[T: Empty](matrix: Matrix[T]) =
    for {
      (position, cellI) <- matrix.cells.toIterator
      cellJ <- adjacentCells(matrix, position)
    } yield (cellI, cellJ, 1.0)

  def distanceDecayNeighbourhoodPairs[T](decay: (Position, Position) => Double)(matrix: Matrix[T]) =
    for {
      (positionI, cellI) <- matrix.cells.toIterator
      (positionJ, cellJ) <- matrix.cells
      if positionI != positionJ
      d = decay(positionI, positionJ)
      if d > 0.0
    } yield (cellI, cellJ, d)

  def capacityMoran[T](matrix: Matrix[T], quantity: Quantity[T]) =
    moran(
      matrix,
      quantity,
      distanceDecayNeighbourhoodPairs[T](1 / distance(_, _))
    )

  def colorRatioMoran(matrix: Matrix[Cell], color: Color) =
    moran(matrix, (c: Cell) => color.cellColor.get(c) / c.population, localNeighbourhoodPairs[Cell])

  def moran[T](state: Matrix[T], quantity: Quantity[T], neighbors: Matrix[T] => Iterator[(T, T, Double)]): Double = {
    def flatCells = state.matrix.flatten
    val totalQuantity = flatCells.map(quantity).sum
    val averageQuantity = totalQuantity / state.numberOfCells

    def numerator =
      neighbors(state).map {
        case (cellI, cellJ, weight) =>
          val term1 = if (quantity(cellI) == 0) 0.0 else (quantity(cellI) - averageQuantity.toDouble)
          val term2 = if (quantity(cellJ) == 0) 0.0 else (quantity(cellJ) - averageQuantity.toDouble)
          weight * term1 * term2
      }.sum

    def denominator =
      flatCells.map {
        cell =>
          if (quantity(cell) == 0) 0
          else math.pow(quantity(cell) - averageQuantity.toDouble, 2)
      }.sum

    val totalWeight = neighbors(state).map { case (_, _, weight) => weight }.sum

    if (denominator.toDouble == 0) 0
    else (state.numberOfCells.toDouble / totalWeight.toDouble) * (numerator / denominator)
  }
}
