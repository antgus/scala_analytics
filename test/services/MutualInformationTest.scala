package services

import org.scalatestplus.play._

class MutualInformationTest extends PlaySpec {

  def epsilonEquals(a : Double, b : Double, epsilon: Double = 0.0001): Boolean = {
    Math.abs(a - b) < epsilon
  }

  def assertComputeMi (dataArray : Array[String] , xVarName :String, yVarName : String, expectedMi: Double): Unit = {
    val data = DataSet.createFromCsvLines(dataArray.iterator)
    assert(epsilonEquals(MutualInformation.computeMi(data, xVarName, yVarName), expectedMi))
    assert(epsilonEquals(MutualInformation.computeMi(data, yVarName, xVarName), expectedMi)) // assert symmetry, mi(x,y)=mi(y,x)
    if(!data.rawDataMatrix.isEmpty) {
      // insert rows with missing values
      var rowWithMissingFirstCell = data.rawDataMatrix(0).clone();
      rowWithMissingFirstCell(0) = DataSet.MissingValue;
      val rowAllMissing = Array.fill[String](data.varNames.length)(DataSet.MissingValue);
      val matrixWithMissingValues = data.rawDataMatrix ++ Array(rowWithMissingFirstCell, rowAllMissing)
      val dataWithMissingValues = new DataSet(data.varNames, matrixWithMissingValues)
      assert(epsilonEquals(MutualInformation.computeMi(dataWithMissingValues, xVarName, yVarName), expectedMi))
      assert(epsilonEquals(MutualInformation.computeMi(dataWithMissingValues, yVarName, xVarName), expectedMi)) // assert symmetry, mi(x,y)=mi(y,x)
    }
    assert(epsilonEquals(MutualInformation.computeMi(data, xVarName, yVarName), expectedMi))
  }

  "MutualInformation" should {
    "be 0 if there is no data" in {
      val dataArray = Array("x,y",",")
      assertComputeMi(dataArray, "x", "y", 0);
    }
    "be zero for data vectors without any variation" in {
      val dataArray = Array(
        "x,y",
        "1,1",
        "1,1"
      )
      assertComputeMi(dataArray, "x", "y", 0)
    }
    "be zero for independent variables" in {
      val dataArray = Array(
        "x,y",
        "1,0",
        "1,1",
        "0,0",
        "0,1"
      );
      assertComputeMi(dataArray, "x", "y", 0)
    }
    "correct when variables can take only two values" in {
      val dataArray = Array(
        "x,y",
        "1,1",
        "1,1",
        "0,0",
        "0,0"
      )
      assertComputeMi(dataArray, "x", "y", 1)
    }
    "correct when variables can take more than 2 values" in {
      val dataArray = Array(
        "x,y",
        "1,1",
        "1,1",
        "0,0",
        "0,0",
        "0,-1",
        "-1,-1"
      )
      assertComputeMi(dataArray, "x", "y", 1.125814583693)
    }
    "correct when variables can take more than 2 values - extended" in {
      /*
       * Example taken from http://www.cl.cam.ac.uk/teaching/0809/InfoTheory/LearnGuide08.pdf
       * Problem set 2, question 2.
       */
      val dataArray = Array(
        "x,y",
        "1,1",
        "1,1",
        "1,1",
        "1,1",
        "2,1",
        "2,1",
        "3,1",
        "3,1",
        "4,1",
        "4,1",
        "4,1",
        "4,1",
        "4,1",
        "4,1",
        "4,1",
        "4,1",
        "1,2",
        "1,2",
        "2,2",
        "2,2",
        "2,2",
        "2,2",
        "3,2",
        "3,2",
        "1,3",
        "2,3",
        "3,3",
        "3,3",
        "1,4",
        "2,4",
        "3,4",
        "3,4"
      )
      assertComputeMi(dataArray, "x", "y", 0.375);
    }
    "correct when calculating pairwise values" in {
      val data = DataSet.createFromCsvLines(Array(
        "x1,x2,x3,x4",
        "1,0,1,0",
        "1,1,1,-1",
        "0,0,0,-1",
        "0,1,0,-1"
      ).iterator)

      val result = MutualInformation.computePairwise(data, "x1")
      result.keys must contain only ("x2","x3","x4")
      assert(epsilonEquals(result.getOrElse("x2", -1), 0))
      assert(epsilonEquals(result.getOrElse("x3", -1), 1))
      assert(epsilonEquals(result.getOrElse("x4", -1), 0.311278124459))

      val result2 = MutualInformation.computePairwise(data, "x2")
      result2.keys must contain only ("x1","x3","x4")
      assert(epsilonEquals(result2.getOrElse("x1", -1), 0))
      assert(epsilonEquals(result2.getOrElse("x3", -1), 0))
      assert(epsilonEquals(result2.getOrElse("x4", -1), 0.311278124459))
    }
  }
}