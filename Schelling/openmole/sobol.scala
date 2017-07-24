logger.level("INFO")

// Inputs
val greenRatio = Val[Double]
val redRatio = Val[Double]
val similarWanted = Val[Double]
val maxCapacity = Val[Double]
val seed = Val[Long]

// Outputs
val step = Val[Int]
val unsatisfiedRatio = Val[Int]
val dissimilarity = Val[Double]
val moran = Val[Double]
val entropy = Val[Double]
val exposureRedGreen = Val[Double]
val exposureGreenRed = Val[Double]
val isolationRedGreen = Val[Double]
val isolationGreenRed = Val[Double]
val deltaRedGreen = Val[Double]
val deltaGreenRed = Val[Double]

val modelTask = 
  ScalaTask("""
    |val simulation = new Schelling with RandomState with RandomMoves with SpeilmanStop {
    |  override def size: Int = 50
    |  override def greenRatio: Double = input.greenRatio
    |  override def redRatio: Double = input.redRatio
    |  override def maxCapacity: Int = input.maxCapacity.toInt
    |  override def similarWanted: Double = input.similarWanted
    |}
    |
    |val result = simulation.run(newRNG(seed))
    |val step = result.step
    |val unsatisfiedRatio = simulation.unsatisifedRatio(result.state)
    |import Color._
    |val dissimilarity = metric.dissimilarity(result.state, Green, Red)
    |val moran = metric.moran(result.state, Red)
    |val entropy = metric.entropy(result.state, Green, Red)
    |val exposureRedGreen = metric.exposureOfColor1ToColor2(result.state, Red, Green)
    |val exposureGreenRed = metric.exposureOfColor1ToColor2(result.state, Green, Red)
    |val isolationRedGreen = metric.isolation(result.state, Red, Green)
    |val isolationGreenRed = metric.isolation(result.state, Green, Red)
    |val deltaRedGreen = metric.delta(result.state, Red, Green)
    |val deltaGreenRed = metric.delta(result.state, Green, Red)
    |""".stripMargin
  ) set (
    imports += "fr.iscpif.schelling.quantity._",
    imports += "move._",
    imports += "initial._",
    imports += "stop._",
    inputs += (greenRatio, redRatio, maxCapacity, similarWanted, seed),
    outputs += (greenRatio, redRatio, maxCapacity, similarWanted, seed),
    outputs += (step, unsatisfiedRatio, dissimilarity, moran, entropy, exposureRedGreen, exposureGreenRed, isolationRedGreen, isolationGreenRed, deltaRedGreen, deltaGreenRed)
  )

val model = Capsule(modelTask)

val explo = ExplorationTask (
      SobolSampling (
        1000,
        greenRatio in Range(0.0, 1.0),
        redRatio in Range(0.0, 1.0),
        similarWanted in Range(0.0, 1.0),
        maxCapacity in Range(1.0,100.0)
      ).filter("greenRatio + redRatio < 0.98") x (seed in UniformDistribution[Long]() take 50)
    )
    
val outputCSV = AppendToCSVFileHook("./schelling.csv")
  
//val env = DIRACEnvironment("biomed", "https://ccdirac06.in2p3.fr:9178", cpuTime = 10 hours, openMOLEMemory = 1500)

val env = DIRACEnvironment("biomed", "https://ccdirac06.in2p3.fr:9178", cpuTime = 4 hours, openMOLEMemory = 1500)
val ex = explo -< (model on env by 200 hook outputCSV) start


