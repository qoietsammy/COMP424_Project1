import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param queries  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], queries: List[String]): List[Double] = {

        // counts the number of times a single query term occurs in the page
        def countQuery(pageText: String, query: String): Double = {
            pageText.split(" ").count(_.toLowerCase() == query.toLowerCase())
        }

        // counts the number of query terms in the entire page
        def countPage(page: RankedWebPage): Double = {
            queries.map(query => countQuery(page.text.toLowerCase(), query)).sum
        }

        // creates a list of each total count
        pages.map(countPage(_))
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val findTotals :List[Double] = pages.map {(page : RankedWebPage) => page.text.split(" ").length.toDouble }
        val findFreq : List[Double] = count(pages,query)
        val tfFound : List[Double] = (findFreq zip findTotals).map{case (freq, total) => freq/total}
        tfFound
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val findFreq = tf(pages, query)
        val idfs = query.map { term =>
          val appearsIn = pages.count(_.text.contains(term)).toDouble
          Math.log(pages.length.toDouble / (appearsIn + 1))
        }
        (findFreq zip idfs).map { case (tf, idf) =>
            tf * idf
        }
    }
}