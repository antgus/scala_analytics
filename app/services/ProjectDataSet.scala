package services

import java.io.File

object ProjectDataSet {

  def load(file : File) : DataSet = {
    val bufferedSource = scala.io.Source.fromFile(file)
    val dataset = DataSet.createFromCsvLines(bufferedSource.getLines())
    bufferedSource.close
    dataset
  }
}
