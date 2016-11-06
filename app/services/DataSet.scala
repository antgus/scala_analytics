package services

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source

/**
  * dataMatrix format:
  * Each row is a data vector
  * Each column is a variable
  * Expected varNames.size = dataMatrix(i).size, for all i.
  * @param varNames
  * @param varDomain
  * @param dataMatrix
  */
class DataSet(val varNames: Array[String], val varDomain: Array[HashMap[String, Int]], val dataMatrix: ArrayBuffer[Array[Int]]) {
  private val mapVarNameToIndex = varNames.zipWithIndex.toMap
  /**
    * Each different variable is a row. Each data vector is a column.
    */
  val transposedDataMatrix = dataMatrix.transpose

  /**
    * Returns the array index that corresponds to the variable.
    * E.g. if there are two variables ["x","y"], getVarIndex("x") returns 0
    * @param varName
    * @return
    */
  def getVarIndex(varName: String) : Option[Int] = {
    mapVarNameToIndex.get(varName)
  }
}

object DataSet {
  /**
    * Expects comma-separated lines
    * The first line has the names of the variables
    * Each subsequent line has a comma-separated data row with the same number of elements
    * Missing values are allowed, represented as empty strings
    * Example:
    * var1, var2, var3
    * 'apples',2,1.2
    * 'oranges',4,1.4
    * @return DataSet
    */
  def createFromCsvLines(lines: Iterator[String]) : DataSet = {
    var isFirst = true
    var varNames = Array[String]()
    var varDomain = Array[HashMap[String, Int]]() // foreach variable, maps to a Hashmap Value -> int
    val dataMatrix = ArrayBuffer[Array[Int]]() // stores data as integer indices
    for (line <- lines) {
      val values = line.split(",",-1).map(_.trim)
      if(isFirst) {
        // headers
        varNames = values
        varDomain = Array.fill(values.length){
          val h = new HashMap[String,Int]
          h.put("", 0)
          h
        }
        isFirst = false
      } else {
        if(values.length != varNames.length) {
          throw new Exception("Length of values " + values.length + " differs from number of variables " + varNames.length)
        }
        val processedRow = values.indices.map( i => {
          if(!varDomain(i).contains(values(i))) {
            // this is a new value for variable i
            varDomain(i).put(values(i), varDomain(i).size)
          }
          varDomain(i).getOrElse(values(i), 0); // replace values by their indices
        }).toArray

        dataMatrix.append(processedRow)
      }
    }
    new DataSet(varNames, varDomain, dataMatrix)
  }
}
