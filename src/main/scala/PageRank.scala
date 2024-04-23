import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*
import scala.language.postfixOps

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.par.map({case(_1,_) => _1 -> 1.0}).seq
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.par.map({case(id: String, page:WebPage) =>
          id -> pages.values.foldLeft(0)((counter : Int, web : WebPage) =>
            if web.links.contains(id) then counter + 1 else counter
          ).toDouble
        }).seq
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val numUsers = 10000
        val numSteps = 100
        val damp = 0.85

        @tailrec
        def simulateSteps(currentPage: String, stepsLeft: Int, acc: Map[String, Double]): Map[String, Double] = {
            if (stepsLeft == 0)
                acc
            else if (Random.nextDouble() < damp) {
                if (pages(currentPage).links.size > 0) then {
                  val nextIndex = Random.nextInt(pages(currentPage).links.size)
                  val nextPage = pages(currentPage).links(nextIndex)
                  simulateSteps(nextPage, stepsLeft - 1, acc.updated(nextPage, acc.getOrElse(nextPage, 0.0) + 1.0))
                } else {
                  simulateSteps(currentPage, stepsLeft - 1, acc.updated(currentPage, acc.getOrElse(currentPage, 0.0) + 1.0))
                }
            } else {
                val nextPage = pages.keys.toList(Random.nextInt(pages.size))
                simulateSteps(nextPage, stepsLeft - 1, acc.updated(nextPage, acc.getOrElse(nextPage, 0.0) + 1.0))
            }
        }
        @tailrec
        def simulateUser(numUsers: Int, acc: Map[String, Double]): Map[String, Double] = {
            if (numUsers == 0)
                acc
            else
                val startPage = pages.keys.toList(Random.nextInt(pages.size))
                val userCounts = simulateSteps(startPage, numSteps, acc)
                simulateUser(numUsers - 1, userCounts)
        }

        val defaultMap = pages.keys.toList.map(key => (key, 0.0)).toMap

        val pageCounts = simulateUser(numUsers, defaultMap).par
        pageCounts.map { case (key, count) =>
            key -> ((count + 1) / (numUsers.toDouble + pages.size))
        }.seq
    }
}