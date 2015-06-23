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

import math._

package object metric {

  def ratio(color: Color, cells: Seq[Cell]) =
    total(color, cells).toDouble / cells.map(_.population).sum

  def total(color: Color, cells: Seq[Cell]) =
    cells.map(color.cellColor.get).sum

  def dissimilarity(state: State, color1: Color, color2: Color): Double = {
    val flatCells = state.matrix.flatten
    val totalPopulation = Seq(color1, color2).map { color => color -> total(color, flatCells) }.toMap

    flatCells.map {
      cell =>
        val nbColor1 = color1.cellColor.get(cell)
        val nbColor2 = color2.cellColor.get(cell)
        abs(nbColor1.toDouble / totalPopulation(color1) - nbColor2.toDouble / totalPopulation(color2))
    }.sum * 0.5
  }

  def entropy(state: State, color1: Color, color2: Color): Double = {
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

  def exposureOfColor1ToColor2(state: State, color1: Color, color2: Color): Double = {
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

  def isolation(state: State, color1: Color, color2: Color): Double = {
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

  def delta(state: State, color1: Color, color2: Color): Double = {
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

  def moran(state: State, color: Color): Double = {
    def flatCells = state.matrix.flatten
    val totalPopulation = flatCells.map(_.population).sum
    val globalColorRatio = flatCells.map(color.cellColor.get).sum.toDouble / totalPopulation.toDouble

    def colorRatio(cell: Cell) = color.cellColor.get(cell).toDouble / cell.population

    def pairs =
      for {
        (position, cellI) <- state.cells
        cellJ <- adjacentCells(state, position)
      } yield (cellI, cellJ)

    def numerator =
      pairs.map {
        case (cellI, cellJ) =>
          val term1 =
            if (cellI.population == 0) 0
            else (colorRatio(cellI) - globalColorRatio.toDouble)
          val term2 =
            if (cellJ.population == 0) 0
            else (colorRatio(cellJ) - globalColorRatio.toDouble)
          term1 * term2
      }.sum

    def denominator =
      flatCells.map {
        cell =>
          if (cell.population == 0) 0
          else pow(colorRatio(cell) - globalColorRatio.toDouble, 2)
      }.sum

    val nbAdjacentCells = pairs.size.toDouble
    val NCells = flatCells.size

    if (denominator.toDouble == 0) 0
    else (NCells.toDouble / nbAdjacentCells.toDouble) * (numerator.toDouble / denominator.toDouble)
  }

  def adjacentCells(state: State, position: Position, size: Int = 1) =
    for {
      oi <- -size to size
      oj <- -size to size
      (i, j) = position
      if i != oi || j != oj
    } yield state(i + oi)(j + oj)

}
