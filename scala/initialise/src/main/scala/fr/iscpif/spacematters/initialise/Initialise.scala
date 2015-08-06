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
package fr.iscpif.spacematters.initialise

import java.io._

import fr.iscpif.mgo._
import fr.iscpif.mgo.mutation._
import fr.iscpif.mgo.crossover._
import fr.iscpif.spacematters.model._
import metric._
import Moran._
import monocle.Lens
import monocle.macros.GenLens

import scala.util.Random

object Initialise extends App {

  val size = 20
  val avgCapacity = 1000

  case class World(size: Int, matrix: Matrix[Int], mutation: Option[Int] = None)

  trait PSE <: NoFitness
    with HitMapArchive
    with GeneticBreeding
    with BinaryTournamentSelection
    with DynamicMutation
    with TournamentOnHitCount
    with HierarchicalRanking
    with RandomNicheElitism
    with CounterTermination
    with ProportionalNumberOfRound
    with PhenotypeGridNiche
    with RandomMating

  trait Aggregated <: Evolution
    with DynamicMutation
    with BinaryTournamentSelection
    with RandomMating
    with TournamentOnAggregatedFitness
    with BestAggregatedElitism
    with NoArchive
    with CloneRemoval
    with GeneticBreeding
    with MGFitness
    with MaxAggregation

  trait NSGAII <: Evolution
    with DynamicMutation
    with BinaryTournamentSelection
    with RandomMating
    with TournamentOnRankAndDiversity
    with NonDominatedElitism
    with FitnessCrowdingDiversity
    with ParetoRanking
    with NonStrictDominance
    with NoArchive
    with CloneRemoval
    with GeneticBreeding
    with MGFitness

  def flatWorld(size: Int, capacity: Int) = World(size, Matrix(Seq.fill(size, size)(capacity)))

  def mutateWorld(world: World, sigma: Double, proportion: Double)(implicit rng: Random) = {
    val buffer = Array.tabulate[Int](world.size, world.size) {
      (i, j) ⇒ world.matrix(i)(j)
    }

    def draw = rng.nextInt(world.size)

    def oneMove = {
      val (fromI, fromJ) = (draw, draw)
      val (toI, toJ) = (draw, draw)
      val fromCap = buffer(fromI)(fromJ)
      val toCap = buffer(toI)(toJ)
      val quantity = {
        val q = math.abs((rng.nextGaussian() * sigma * fromCap).toInt)
        math.min(fromCap, q)
      }

      buffer(toI)(toJ) = toCap + quantity
      buffer(fromI)(fromJ) = fromCap - quantity
    }

    val nbMoves = (world.size * world.size) * proportion

    for (m ← 0 until rng.nextInt(nbMoves.toInt + 1)) oneMove
    world.copy(matrix = Matrix(buffer.map(_.toSeq)))
  }

  case class MatrixEvaluation(slope: Double, r2: Double, distance: Double, moran: Double, entropy: Double)

  case class Bound(min: Double, max: Double) {
    def middle = (max - min) / 2.0
    def contains(v: Double) = min <= v && max >= v
    def distance(v: Double) =
      if (v < min) math.abs(v - min)
      else if (v > max) math.abs(v - max)
      else 0.0
  }

  val moranBound = Bound(0.0, 0.025)
  val distanceBound = Bound(0.0, 0.02)
  val entropyBound = Bound(0.7, 0.95)
  val slopeBound = Bound(-2, -1)
  val r2Bound = Bound(0.5, 1.0)

  def valid(matrixEvaluation: MatrixEvaluation) = {
    import matrixEvaluation._
    moranBound.contains(moran) &&
      distanceBound.contains(distance) &&
      entropyBound.contains(entropy) &&
      slopeBound.contains(slope) &&
      r2Bound.contains(r2)
  }

  def evaluateMatrix(matrix: Matrix[Int]) = {
    def id = (x: Int) ⇒ x.toDouble
    val (s, r2) = slope(matrix, id)

    MatrixEvaluation(
      slope = s,
      r2 = r2,
      distance = distanceMean(matrix, id),
      moran = capacityMoran(matrix, id),
      entropy = entropy(matrix, id)
    )
  }

  def trash =
    MatrixEvaluation(
      Double.NegativeInfinity,
      Double.NegativeInfinity,
      Double.NegativeInfinity,
      Double.NegativeInfinity,
      Double.NegativeInfinity)

  def evaluateMatrixPSE(matrix: Matrix[Int]) = {
    val evaluation = evaluateMatrix(matrix)
    if (!valid(evaluation)) trash
    else evaluation
  }

  trait WorldEvolution <: G with P with F with A with Mutation {
    type G = World
    def parameterableMutation(sigma: Double, cellRatio: Double) =
      (g: G, p: Population[G, P, F], a: A, rng: Random) ⇒ mutateWorld(g, sigma, cellRatio)(rng)

    def mutations: Vector[Mutation] =
      Vector(
        //randomMutation,
        parameterableMutation(0.5, 0.25),
        parameterableMutation(0.2, 0.10),
        parameterableMutation(0.1, 0.05),
        parameterableMutation(0.05, 0.05)
      )

    def randomGenome(implicit rng: Random): World =
      mutateWorld(
        flatWorld(size, avgCapacity),
        0.5,
        0.5)(rng)

    def fromMutation: Lens[World, Option[Int]] = GenLens[World](_.mutation)
  }

  val searchFirst = new Aggregated with WorldEvolution with Problem with ConditionalTermination {

    override def mu = 500
    override def lambda = 200

    type P = MatrixEvaluation

    def express(g: World, rng: Random) = evaluateMatrix(g.matrix)

    override def evaluate(phenotype: P, rng: Random): Seq[Double] = {
      val slopeDiff = slopeBound.distance(phenotype.slope)
      val moranDiff = moranBound.distance(phenotype.moran)
      val distanceDiff = distanceBound.distance(phenotype.distance)
      val entropyDiff = entropyBound.distance(phenotype.entropy)
      val r2Diff = r2Bound.distance(phenotype.r2)
      Seq(slopeDiff, moranDiff, distanceDiff, entropyDiff, r2Diff)
    }

    override def maxSteps: Int = 100000
    override def terminated(population: Population[World, MatrixEvaluation, Seq[Double]])(implicit rng: Random): Boolean =
      population.exists {
        i ⇒ valid(i.phenotype)
      }

    def crossovers: Vector[Crossover] = Vector()
  }

  implicit val rng = new Random(42)

  val validGenomes =
    searchFirst.evolve.untilConverged {
      p ⇒
        if (p.population.isEmpty) println(s"${p.generation}: empty pop")
        else {
          val mins = p.population.map(_.fitness).transpose.map(_.min)
          println(p.generation + ": " + mins.mkString(","))
        }
    }.population.map(_.genome)

  val pse = new PSE with WorldEvolution {

    def express(g: World, rng: Random): Seq[Double] = {
      val eval = evaluateMatrixPSE(g.matrix)
      Seq(eval.slope, eval.moran, eval.distance, eval.entropy)
    }

    override def steps: Int = 10000
    override def lambda: Int = 1000

    override def gridSize: Seq[Double] = Seq(0.02, 0.002, 0.02, 0.1)

    def crossovers: Vector[Crossover] = Vector()
  }

  val result = new File(args(0))
  result.mkdir()

  //searchFirst.evolve().untilConverged { s ⇒ s }

  val res = pse.evolve(validGenomes).untilConverged {
    s ⇒
      val dir = new File(result, s.generation.toString)
      dir.mkdirs

      def saveGrid(world: World, p: Seq[Double], name: String) = {
        val file = new File(dir, name)
        val fos = new PrintStream(new BufferedOutputStream(new FileOutputStream(file)))
        try {
          def str = world.matrix.matrix.map(_.mkString(",")).mkString("\n")
          fos.println(p.mkString(","))
          fos.println(str)
        } finally fos.close
      }

      s.population.zipWithIndex.foreach {
        case (indiv, i) ⇒ saveGrid(indiv.genome, indiv.phenotype, i.toString)
      }

      val intervals =
        s.population.map(_.phenotype).transpose.map {
          s ⇒
            val fitred = s.filter(_ != Double.NegativeInfinity)
            if (fitred.isEmpty) (0.0, 0.0)
            else fitred.min -> fitred.max
        }

      println(s.terminationState + " " + s.population.size + " " + intervals.mkString(","))
  }

}
