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

import fr.iscpif.spacematters.model.move.Neighborhood
import org.apache.commons.math3.stat.regression.SimpleRegression
import fr.iscpif.spacematters.model._
import math._

package object metric {

  def ratio(color: Color, cells: Seq[Cell]) =
    total(color, cells).toDouble / cells.map(_.population).sum

  def total(color: Color, cells: Seq[Cell]) =
    cells.map(color.cellColor.get).sum

  def dissimilarity(state: Matrix[Cell], color1: Color, color2: Color): Double = {
    val flatCells = state.matrix.flatten
    val totalPopulation = Seq(color1, color2).map { color => color -> total(color, flatCells) }.toMap

    flatCells.map {
      cell =>
        val nbColor1 = color1.cellColor.get(cell)
        val nbColor2 = color2.cellColor.get(cell)
        abs(nbColor1.toDouble / totalPopulation(color1) - nbColor2.toDouble / totalPopulation(color2))
    }.sum * 0.5
  }

  def segregationEntropy(state: Matrix[Cell], color1: Color, color2: Color): Double = {
    val flatCells = state.matrix.flatten
    val totalPopulation = Seq(color1, color2).map { color => color -> total(color, flatCells) }.toMap
    val totalPropColor1 = totalPopulation(color1).toDouble / (totalPopulation(color1).toDouble + totalPopulation(color2).toDouble)
    val totalPropColor2 = totalPopulation(color2).toDouble / (totalPopulation(color1).toDouble + totalPopulation(color2).toDouble)

    val logInversePropColor1 =
      if (totalPropColor1.toDouble == 0) 0
      else math.log(1 / totalPropColor1.toDouble)

    val logInversePropColor2 =
      if (totalPropColor2.toDouble == 0) 0
      else math.log(1 / totalPropColor2.toDouble)

    val cityEntropy = (totalPropColor1.toDouble * logInversePropColor1.toDouble) + (totalPropColor2.toDouble * logInversePropColor2.toDouble)

    flatCells.map {
      cell =>
        val nbColor1 = color1.cellColor.get(cell)
        val nbColor2 = color2.cellColor.get(cell)
        val cellPop = nbColor1.toDouble + nbColor2.toDouble

        val cellPropColor1 =
          if (cellPop.toDouble == 0) 0
          else nbColor1.toDouble / cellPop.toDouble
        val cellPropColor2 =
          if (cellPop.toDouble == 0) 0
          else nbColor2.toDouble / cellPop.toDouble

        val logInverseCellPropColor1 =
          if (cellPropColor1.toDouble == 0) 0
          else math.log(1 / cellPropColor1.toDouble)
        val logInverseCellPropColor2 =
          if (cellPropColor2.toDouble == 0) 0
          else math.log(1 / cellPropColor2.toDouble)

        val cellEntropy = (cellPropColor1.toDouble * logInverseCellPropColor1.toDouble) + (cellPropColor2.toDouble * logInverseCellPropColor2.toDouble)

        (cellPop.toDouble * (cityEntropy.toDouble - cellEntropy.toDouble)) / (cityEntropy.toDouble * (totalPopulation(color1).toDouble + totalPopulation(color2).toDouble))

    }.sum
  }

  def exposureOfColor1ToColor2(state: Matrix[Cell], color1: Color, color2: Color): Double = {
    val flatCells = state.matrix.flatten
    val totalPopulation = Seq(color1, color2).map { color => color -> total(color, flatCells) }.toMap
    val totalPopColor1 = totalPopulation(color1).toDouble

    flatCells.map {
      cell =>
        val nbColor1 = color1.cellColor.get(cell)
        val nbColor2 = color2.cellColor.get(cell)
        val cellPop = nbColor1.toDouble + nbColor2.toDouble

        val cellPropColor2 =
          if (cellPop.toDouble == 0) 0
          else nbColor2.toDouble / cellPop.toDouble

        (nbColor1.toDouble / totalPopColor1.toDouble) * cellPropColor2.toDouble

    }.sum
  }

  def isolation(state: Matrix[Cell], color1: Color, color2: Color): Double = {
    val flatCells = state.matrix.flatten
    val totalPopulation = Seq(color1, color2).map { color => color -> total(color, flatCells) }.toMap
    val totalPopColor1 = totalPopulation(color1).toDouble

    flatCells.map {
      cell =>
        val nbColor1 = color1.cellColor.get(cell)
        val nbColor2 = color2.cellColor.get(cell)
        val cellPop = nbColor1.toDouble + nbColor2.toDouble

        val cellPropColor1 =
          if (cellPop.toDouble == 0) 0
          else nbColor1.toDouble / cellPop.toDouble

        (nbColor1.toDouble / totalPopColor1.toDouble) * cellPropColor1.toDouble

    }.sum
  }

  def delta(state: Matrix[Cell], color1: Color, color2: Color): Double = {
    val flatCells = state.matrix.flatten
    val totalPopulation = Seq(color1, color2).map { color => color -> total(color, flatCells) }.toMap
    val totalPopColor1 = totalPopulation(color1).toDouble
    val NCells = flatCells.size

    flatCells.map {
      cell =>
        val nbColor1 = color1.cellColor.get(cell)

        abs((nbColor1.toDouble / totalPopColor1.toDouble) - (1 / NCells.toDouble))

    }.sum * 0.5
  }

  def slope[T](matrix: Matrix[T], quantity: Quantity[T]) = {
    def distribution = matrix.flatten.map(quantity).sorted(Ordering.Double.reverse).filter(_ > 0)
    def distributionLog = distribution.zipWithIndex.map { case (q, i) => Array(log(i + 1), log(q)) }.toArray
    val simpleRegression = new SimpleRegression(true)
    simpleRegression.addData(distributionLog)
    (simpleRegression.getSlope(), simpleRegression.getRSquare())
  }

  def distance(p1: Position, p2: Position): Double = {
    val (i1, j1) = p1
    val (i2, j2) = p2
    val a = i2 - i1
    val b = j2 - j1
    math.sqrt(a * a + b * b) ///hypot(i2 - i1, j2 - j1)
  }

  def distanceMean[T](matrix: Matrix[T], quantity: Quantity[T]) = {

    def totalQuantity = matrix.flatten.map(quantity).sum

    def numerator =
      (for {
        (c1, p1) <- matrix.zipWithPosition
        (c2, p2) <- matrix.zipWithPosition
      } yield distance(p1, p2) * quantity(c1) * quantity(c2)).sum

    def normalisation = matrix.side / math.sqrt(math.Pi)

    (numerator / (totalQuantity * (totalQuantity - 1))) / normalisation
  }

  def entropy[T](matrix: Matrix[T], quantity: Quantity[T]) = {
    val totalQuantity = matrix.flatten.map(quantity).sum
    assert(totalQuantity > 0)
    matrix.flatten.map {
      p =>
        val quantityRatio = quantity(p) / totalQuantity
        val localEntropy = if (quantityRatio == 0.0) 0.0 else quantityRatio * math.log(quantityRatio)
        assert(!localEntropy.isNaN, s"${quantityRatio} ${math.log(quantityRatio)}")
        localEntropy
    }.sum * (-1 / math.log(matrix.numberOfCells))
  }

  def adjacentCells[T: Empty](state: Matrix[T], position: Position, size: Int = 1) =
    for {
      oi <- -size to size
      oj <- -size to size
      (i, j) = position
      if i != oi || j != oj
    } yield state(i + oi)(j + oj)

}
