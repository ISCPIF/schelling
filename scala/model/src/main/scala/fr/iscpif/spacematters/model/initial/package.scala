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

import java.io.File

import com.nrinaudo._

import scala.collection.mutable

package object initial {

  def readMatrix(f: File, size: Int): Seq[Seq[Int]] = {
    val capacities = csv.safe(f, ',').drop(1)
    val matrix = Array.fill[Int](size, size)(0)

    for {
      Vector(i, j, c) ← capacities
    } matrix(i.toInt)(j.toInt) = c.toDouble.toInt

    matrix.map(_.toSeq).toSeq
  }

  def normalize(matrix: Seq[Seq[Int]], population: Int) = {
    val total = matrix.flatten.sum
    val ratio = population.toDouble / total
    matrix.map(_.map(v ⇒ (v * ratio).toInt))
  }

}
