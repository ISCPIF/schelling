package fr.iscpif.spacematters.model.container

import fr.iscpif.spacematters.model.Cell

import scala.util.Random

trait ExpMixtureContainer <: Container {

  /** maximal capacity C_m */
  def maxCapacity: Int

  /** Size of exponential kernels, of the form C_m*exp(-||x-x_0||/r_0) */
  def kernelRadius: Double

  /** Number of exponential kernels */
  def centersNumber: Int

  def container(implicit rng: Random) = {
    val arrayVals = Array.fill[Cell](size, size) {
      new Cell(0, 0, 0)
    }

    // generate random center positions
    val centers = Array.fill[Int](centersNumber, 2) {
      rng.nextInt(size)
    }

    for (i <- 0 to size - 1; j <- 0 to size - 1) {
      for (c <- 0 to centersNumber - 1) {
        arrayVals(i)(j).capacity = arrayVals(i)(j).capacity + maxCapacity * math.exp(-math.sqrt(math.pow((i - centers(c)(0)), 2) + math.pow((j - centers(c)(1)), 2)) / kernelRadius)
      }
    }

    Seq.tabulate(size, size) { (i: Int, j: Int) => Cell(arrayVals(i)(j).capacity, 0, 0) }

  }

}
