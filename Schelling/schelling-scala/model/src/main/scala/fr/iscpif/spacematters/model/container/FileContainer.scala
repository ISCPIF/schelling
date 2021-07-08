package fr.iscpif.spacematters.model.container

import fr.iscpif.spacematters.model.Cell

import scala.io.Source
import scala.util.Random

//import java.io.File

trait FileContainer <: Container {

  def configfile: String

  def container(implicit rng: Random) = {

    println("initialising from file " + configfile)

    val lines = Source.fromFile(configfile).getLines().drop(1).map(_.split(",").map(_.toDouble))

    val arrayVals = Array.fill[Cell](size, size) {
      new Cell(0, 0, 0)
    }

    for (line <- lines) {
      arrayVals(line(0).toInt - 1)(line(1).toInt - 1).capacity = line(2)
    }

    Seq.tabulate(size, size) { (i: Int, j: Int) => Cell(arrayVals(i)(j).capacity, 0, 0) }

  }

}
