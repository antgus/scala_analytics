import java.io.File

import com.github.tototoshi.csv.CSVReader
import play.api.ApplicationLoader.Context
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.routing.Router
import play.api.routing.sird._

import collection.mutable.{ArrayBuffer, HashMap}
import play.Environment
import services.{DataSet, MutualInformation}

import scala.concurrent.Future

class AppLoader extends ApplicationLoader {

  def load(context: Context) = new BuiltInComponentsFromContext(context) {

    val dataset = { // load dataset only once. Not bothering with try-catch-finally
      val reader = CSVReader.open(new File("conf/old_projects_dataset.csv"))
      val dataset = DataSet.createFromIterator(reader.iterator)
      reader.close()
      dataset
    }

    val router = Router.from {
      case GET(p"/dependencies" ? q_o"variable=$queryVar") => Action.async {
        val chosenVarName = queryVar.getOrElse("")
        if(queryVar.isDefined) {
          if(!dataset.varNames.contains(chosenVarName)) {
            Future {UnprocessableEntity("Queried variable="+chosenVarName+" is not valid. " + "You can pick one of the following:\n" + dataset.varNames.sorted.mkString("\n"))}
          } else {
            // for the given variable x=chosenVarName, and all other variables y != x, compute mutual information mi(x,y)
            val miMap = (MutualInformation.computePairwise(dataset, chosenVarName) - "id")
            val output = "mi,variable\n" + miMap.toSeq.sortWith(_._2 > _._2).map(x => x._2 + "," + x._1).mkString("\n")
            Future {Ok(output).as("text/csv")}
          }
        } else {
          Future {
            UnprocessableEntity("Please specify the variable for which you wish to compute the pairwise mutual information metric.\n" +
              "Example: ?variable=customer_is_hands_on\n" +
              "You can pick one of the following:\n" + dataset.varNames.sorted.mkString("\n"))
          }
        }
      }
    }
  }.application

}