package fr.iscpif.spacematters.model.test

import fr.iscpif.spacematters.model._
import fr.iscpif.spacematters.model.container._
import fr.iscpif.spacematters.model.initial._
import fr.iscpif.spacematters.model.move._
import fr.iscpif.spacematters.model.stop._

import scala.util.Random

object TestContainer extends App {

  val t = System.currentTimeMillis()

  implicit val rng = new Random

  val simulation = new Schelling with RandomState with PrefAttDiffusionContainer with RandomMoves with SpeilmanStop {
    override def size: Int = 50
    override def greenRatio: Double = 0.5
    override def redRatio: Double = 0.35
    override def maxCapacity: Int = 50
    override def similarWanted: Double = 0.4

    /*// exp Mixture params
    override def kernelRadius = 0.5
    override def centersNumber = 2
    */

    //prefAttdiff
    override def totalCapacity :Double = 10000
    override def diffusion : Double = 0.02
    override def diffusionSteps : Int = 2
    override def growthRate : Int = 100
    override def alphaAtt : Double = 1.1
  }

  // generate initial state
  simulation.initialState(rng)

  // print container
  //simulation.printContainer

  println(simulation.values.flatten.map(_.capacity).max)

  println((System.currentTimeMillis()-t)/1000.0)

}
