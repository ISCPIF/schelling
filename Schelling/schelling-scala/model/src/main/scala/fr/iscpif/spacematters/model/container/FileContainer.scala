package fr.iscpif.spacematters.model.container

import fr.iscpif.spacematters.model.Cell

import scala.io.Source

//import java.io.File

trait FileContainer <: Container {

  def configfile: String

  def container(implicit rng: Random) = {

    val lines = Source.fromFile(configfile).getLines
    
    Seq.fill(size, size) {
      val capacity: Double = rng.nextInt(maxCapacity).toDouble
      Cell(capa = capacity, green = 0, red = 0)
    }
  }

}
