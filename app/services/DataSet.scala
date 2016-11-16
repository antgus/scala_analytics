package services

import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
  * dataMatrix format:
  * Each row is a data vector
  * Each column is a variable
  *
  * Note: A value = DataSet.MissingValue is considered to be a missing value.
  *
  * @throws RuntimeException if varNames.size != dataMatrix(i).size, for any i
  * @param varNames
  * @param rawDataMatrix
  */
class DataSet(val varNames: Seq[String], val rawDataMatrix: ArrayBuffer[Array[String]]) {
  private val mapVarNameToIndex = varNames.zipWithIndex.toMap

  val varDomain: Array[HashMap[String, Int]] = {
    // foreach variable, maps to a Hashmap Value -> int
    val varDomain = Array.fill(varNames.length) {
      val h = new HashMap[String, Int]
      h.put(DataSet.MissingValue, 0)
      h
    }
    for (row <- rawDataMatrix) {
      if (row.length != varNames.length) throw new RuntimeException("Expected rows to be the same length as varNames array. Found: row.length=" + row.length + " varNames.length=" + varNames.length)
      for (i <- row.indices) {
        if (!varDomain(i).contains(row(i))) {
          // this is a new value for variable i
          varDomain(i).put(row(i), varDomain(i).size)
        }
      }
    }
    varDomain
  }

  /*
   * Replace the original rawDataMatrix where each entry is a string to an integer-only version of the data matrix where each
   * entry is an index that maps to the original value through varDomain
   *
   * For example original:
   * ["a1","b1"]
   * ["a2',"b2"]
   * ["a3',"b1"]
   * ->
   * [0,0]
   * [1,1]
   * [2,0]
   *
   * with varDomain(0) = ["a1" -> 0, "a2" -> 1, "a3" -> 2]
   * and varDomain(1) = ["b1" -> 0, "b2" -> 1]
   */
  val dataMatrix = rawDataMatrix.map(row => row.indices.map(i => varDomain(i).getOrElse(row(i), throw new AssertionError("Logic should guarantee map entry exists"))).toArray)

  /**
    * Each different variable is a row. Each data vector is a column.
    */
  val transposedDataMatrix = dataMatrix.transpose

  /**
    * Returns the array index that corresponds to the variable.
    * E.g. if there are two variables ["x","y"], getVarIndex("x") returns 0
    *
    * @param varName
    * @return
    */
  def getVarIndex(varName: String): Option[Int] = {
    mapVarNameToIndex.get(varName)
  }
}

object DataSet {
  val MissingValue = ""

  def createFromIterator(lines: Iterator[Seq[String]]): DataSet = {
    val varNames = lines.next()
    val dataMatrix = ArrayBuffer[Array[String]]()
    for(line <- lines) {
      dataMatrix.append(line.toArray)
    }
    new DataSet(varNames, dataMatrix)
  }
  /**
    * Expects comma-separated lines
    * The first line has the names of the variables
    * Each subsequent line has a comma-separated data row with the same number of elements
    * Missing values are allowed, represented as empty strings
    * Example:
    * var1, var2, var3
    * 'apples',2,1.2
    * 'oranges',4,1.4
    *
    * @return DataSet
    */
  def createFromCsvLines(lines: Iterator[String]): DataSet = {
    var isFirst = true
    var varNames = Array[String]()
    val dataMatrix = ArrayBuffer[Array[String]]()
    for (line <- lines) {
      val values = line.split(",", -1).map(_.trim)
      if (isFirst) {
        // headers
        varNames = values
        isFirst = false
      } else {
        if (values.length != varNames.length) {
          throw new Exception("Length of values " + values.length + " differs from number of variables " + varNames.length)
        }
        dataMatrix.append(values)
      }
    }
    new DataSet(varNames, dataMatrix)
  }
}
