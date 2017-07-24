package fr.iscpif.spacematters.model.container

import fr.iscpif.spacematters.model.Cell

import scala.util.Random

trait PrefAttDiffusionContainer <: Container {

  /** sum of all capacities */
  def totalCapacity: Double

  /** diffusion parameters */
  def diffusion: Double
  def diffusionSteps: Int

  /** Growth rate */
  def growthRate: Int

  /** Preferential attachment parameter */
  def alphaAtt: Double

  /**
   *
   * @param rng
   * @return
   */
  def container(implicit rng: Random) = {
    var arrayVals = Array.fill[Cell](size, size) { new Cell(0, 0, 0) }
    var population: Double = 0

    while (population < totalCapacity) {

      // add new population following pref att rule
      if (population == 0) {
        //choose random patch
        for (_ <- 1 to growthRate) { val i = rng.nextInt(size); val j = rng.nextInt(size); arrayVals(i)(j).capacity = arrayVals(i)(j).capacity + 1 }
      } else {
        val oldPop = arrayVals.clone()
        val ptot = oldPop.flatten.map((c: Cell) => math.pow(c.capacity / population, alphaAtt)).sum

        /*
          val drawings = Array.fill[Double](growthRate){rng.nextDouble()}
          Sorting.quickSort(drawings)
          var s = 0.0; var i =0;var j =0;var k=0;
          oldPop.flatten.foreach(
            (c:Cell)=>{
              s=s+(math.pow(c.capacity/population,alphaAtt)/ptot)
              while(s<drawings(k)){arrayVals(i)(j).capacity = arrayVals(i)(j).capacity + 1;k=k+1;}
              j=j+1;if(j==size){j=0;i=i+1};
            }
          )
          */

        for (_ <- 1 to growthRate) {
          var s = 0.0; val r = rng.nextDouble(); var i = 0; var j = 0
          while (s < r) { s = s + (math.pow(oldPop(i)(j).capacity / population, alphaAtt) / ptot); j = j + 1; if (j == size) { j = 0; i = i + 1 } }
          if (j == 0) { j = size - 1; i = i - 1 } else { j = j - 1 };
          arrayVals(i)(j).capacity = arrayVals(i)(j).capacity + 1
        }
      }

      // diffuse
      for (_ <- 1 to diffusionSteps) {
        arrayVals = diffuse(arrayVals, diffusion)
      }

      // update total population
      population = arrayVals.flatten.map(_.capacity).sum

      //println("population : "+population)
    }

    Seq.tabulate(size, size) { (i: Int, j: Int) => Cell(arrayVals(i)(j).capacity, 0, 0) }

  }

  /**
   * Diffuse to neighbors proportion alpha of capacities
   * @param a
   */
  def diffuse(a: Array[Array[Cell]], alpha: Double): Array[Array[Cell]] = {
    val newVals = a.clone()
    for (i <- 0 to size - 1; j <- 0 to size - 1) {
      if (i >= 1) { newVals(i - 1)(j).capacity = newVals(i - 1)(j).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
      if (i < size - 1) { newVals(i + 1)(j).capacity = newVals(i + 1)(j).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
      if (j >= 1) { newVals(i)(j - 1).capacity = newVals(i)(j - 1).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
      if (j < size - 1) { newVals(i)(j + 1).capacity = newVals(i)(j + 1).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
      if (i >= 1 && j >= 1) { newVals(i - 1)(j - 1).capacity = newVals(i - 1)(j - 1).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
      if (i >= 1 && j < size - 1) { newVals(i - 1)(j + 1).capacity = newVals(i - 1)(j + 1).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
      if (i < size - 1 && j >= 1) { newVals(i + 1)(j - 1).capacity = newVals(i + 1)(j - 1).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
      if (i < size - 1 && j < size - 1) { newVals(i + 1)(j + 1).capacity = newVals(i + 1)(j + 1).capacity + (alpha / 8) * a(i)(j).capacity; newVals(i)(j).capacity = newVals(i)(j).capacity - (alpha / 8) * a(i)(j).capacity }
    }
    newVals
  }

}
