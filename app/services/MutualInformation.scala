package services

object MutualInformation {

  /**
    * Computes the mutual information between two vars x,y.
    * MI(x,y) = for all x,y sum p(x,y) log(p(x,y) / p(x)p(y))
    * For more information check: https://en.wikipedia.org/wiki/Mutual_information
    * @param dataset
    * @param xName
    * @param yName
    * @return
    */
  def computeMi(dataset: DataSet, xName: String, yName: String) : Double = {
    if(dataset.dataMatrix.isEmpty)
      0
    else
      doComputeMi(dataset, xName, yName)
  }

  private def doComputeMi(dataset: DataSet, xName: String, yName: String): Double = {
    val transposedMatrix = dataset.transposedDataMatrix
    val x = dataset.getVarIndex(xName).get
    val y = dataset.getVarIndex(yName).get
    val xRow = transposedMatrix(x)
    val yRow = transposedMatrix(y)

    // Note: the following pairwiseCounter is quadratic in the number of unique variable values, i.e. O(|X|*|Y|)
    // where |X| is the number of unique values variable x can take
    // Alternative implementation with hashes for unique pairs would be O(N), where N is the number of rows.
    // Hashes will have slower access but will be faster if |X|*|Y| is >> N
    val xCounter = Array.fill[Int](dataset.varDomain(x).size)(0)
    val yCounter = Array.fill[Int](dataset.varDomain(y).size)(0)
    val xyCounter = Array.fill(dataset.varDomain(x).size) {
      Array.fill[Int](dataset.varDomain(y).size)(0)
    }
    for (i <- xRow.indices) {
      if (yRow(i) != 0 && xRow(i) != 0) {
        // only consider rows where both columns are defined
        xCounter(xRow(i)) += 1
        yCounter(yRow(i)) += 1
        xyCounter(xRow(i))(yRow(i)) += 1
      }
    }
    val xSum = xCounter.sum
    val xProbDist = xCounter.map(x => x.toDouble / xSum)
    val ySum = yCounter.sum
    val yProbDist = yCounter.map(x => x.toDouble / ySum)
    val xySum = xyCounter.map(_.sum).sum
    val xyProbDist = xyCounter.map(x => x.map(y => y.toDouble / xySum))
    var mutualInformation = 0.0
    for (i <- xyProbDist.indices; j <- xyProbDist(i).indices) {
      // Mutual information = sum over all x,y:  p(x,y) log2(p(x,y) / (p(x)*p(y))
      val v = xyProbDist(i)(j) * log2(xyProbDist(i)(j) / (xProbDist(i) * yProbDist(j)))
      mutualInformation += (if (v.isNaN) 0 else v)
    }
    mutualInformation
  }

  /**
    * Computes the mutual information MI(x,y) for all variables y != xName
    * @return map [y => MI(x,y)], where y is the variable name
    */
  def computePairwise(dataset: DataSet, xName: String) : Map[String, Double] = {
    dataset.varNames.filter(_ != xName).
      map(yName => (yName,computeMi(dataset, xName, yName))).
      toMap
  }

  private def log2(x: Double): Double = Math.log(x) / Math.log(2)
}
