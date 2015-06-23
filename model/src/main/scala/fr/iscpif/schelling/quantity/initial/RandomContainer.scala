package fr.iscpif.schelling.quantity.initial

import fr.iscpif.schelling.quantity.Cell

import scala.util.Random


trait RandomContainer <: Container {

   def maxCapacity : Int

   var values : Seq[Seq[Cell]] = null

   def container(implicit rng: Random) = {
     values = Seq.fill(size, size) {
       val capacity: Double = rng.nextInt(maxCapacity).toDouble
       Cell(capa = capacity, green = 0, red = 0)
     }
     values
   }

}
