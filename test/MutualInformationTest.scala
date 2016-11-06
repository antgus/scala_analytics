import org.scalatestplus.play._
import org.scalactic.TolerantNumerics
import services.{DataSet, MutualInformation}

import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
  * Add your spec here.
  * You can mock out a whole application including requests, plugins etc.
  * For more information, consult the wiki.
  */
class MutualInformationTest extends PlaySpec {

  def epsilonEquals(a : Double, b : Double, epsilon: Double = 0.0001): Boolean = {
    Math.abs(a - b) < epsilon
  }

  "MutualInformation" should {
    "be zero for data vectors without any variation" in {
      val data = DataSet.createFromCsvLines(Array(
        "x,y",
        "1,1",
        "1,1"
      ).iterator)

      assert(epsilonEquals(MutualInformation.computeMi(data, "x", "y"), 0))
      assert(epsilonEquals(MutualInformation.computeMi(data, "y", "x"), 0))
    }
    "be zero for independent variables" in {
      val data = DataSet.createFromCsvLines(Array(
        "x,y",
        "1,0",
        "1,1",
        "0,0",
        "0,1"
      ).iterator)

      assert(epsilonEquals(MutualInformation.computeMi(data, "x", "y"), 0))
      assert(epsilonEquals(MutualInformation.computeMi(data, "y", "x"), 0))
    }
    "be correct" in {
      val data = DataSet.createFromCsvLines(Array(
        "x,y",
        "1,1",
        "1,1",
        "0,0",
        "0,0"
      ).iterator)

      assert(epsilonEquals(MutualInformation.computeMi(data, "x", "y"), 1))
      assert(epsilonEquals(MutualInformation.computeMi(data, "y", "x"), 1))
    }
    "correct when calculating pairwise values" in {
      val data = DataSet.createFromCsvLines(Array(
        "x1,x2,x3",
        "1,0,1",
        "1,1,1",
        "0,0,0",
        "0,1,0"
      ).iterator)

      val result = MutualInformation.computePairwise(data, "x1")
      result.keys must contain only ("x2","x3")
      assert(epsilonEquals(result.getOrElse("x2", -1), 0))
      assert(epsilonEquals(result.getOrElse("x3", -1), 1))
    }
  }
}