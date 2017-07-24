package fr.iscpif.spacematters.model.container

import fr.iscpif.spacematters.model.Cell

import scala.util.Random

trait RandomContainer <: Container {

  def maxCapacity: Int

  def container(implicit rng: Random) = {
    Seq.fill(size, size) {
      val capacity: Double = rng.nextInt(maxCapacity).toDouble
      Cell(capa = capacity, green = 0, red = 0)
    }
  }

}
