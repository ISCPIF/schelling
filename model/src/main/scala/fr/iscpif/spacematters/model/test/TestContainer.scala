package fr.iscpif.spacematters.model.test

import fr.iscpif.spacematters.model.container._

import scala.util.Random

object TestContainer extends App {

  val t = System.currentTimeMillis()

  implicit val rng = new Random

  val cont = new  PrefAttDiffusionContainer{
    override def size: Int = 50

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

  // generate instance of container
  println(cont)
  // an other
  println(cont)

  //println((System.currentTimeMillis()-t)/1000.0)

}
