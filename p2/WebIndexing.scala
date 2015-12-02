import java.io.{ BufferedWriter, File, FileWriter }

import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.dal.tinyir.processing.{ TipsterCorpusIterator, Tokenizer }

import scala.io.Source
import scala.math._
import scala.collection.mutable.{ HashMap, PriorityQueue }
import scala.math.Ordering.Implicits._

object WebIndexing {

  def main(args: Array[String]): Unit = {
    /* Phase 0. prepare queries
     * get all terms appeared in queries `allQueryTerms`
     */
    val stopWords = Set("the", "a", "is", "it", "are", "of", "in", "from", "to", "for", "by", "on", "across", "against")
    val queries =
      Source.fromFile("topics").getLines
        .filter(_.contains("<title>")) // keep only lines starting with "<title>"
        .map(_.toLowerCase
          .replaceAll("<title>\\s*topic:", "") // remove leading string
          .replaceAll("[,()\"`&/-]", " ").replaceAll("\\s+", " ") // do NOT replace '.' here (for the U.S. case...) 
          .replaceAll("^ *", "").replaceAll(" *$", "") // strip leading/trailing spaces
          ).toList // List[String]

    val queryTerms = queries.map(_.split(" +").filter(!stopWords.contains(_))) // List[Array[String]]
    val allQueryTerms = queryTerms.flatten.distinct // List[String], length=121

    /* Phase 1. reading documents
     * To apply term-based model/language model, we need:
     * tf(w,d) --HashMap[String, Int] (instead of `HashMap[Array[String], Int]`)
     * df(w) --HashMap[String, Int]
     * cf(w) --HashMap[String, Int]
     * len(d) := length of document d --HashMap[String, Int] 
     * N := nb of documents
     * totalLen := length of all docs (sum of cf(v) for all v)
     */
    var tf = new HashMap[String, Int] // use 'term&docname' as the key
    var df = new HashMap[String, Int]
    var cf = new HashMap[String, Int]
    var len = new HashMap[String, Int]
    var N = 0
    var totalLen = 0

    val iterator = new TipsterCorpusIterator("../zips/zips-tiny")
    while (iterator.hasNext) { //*** main loop for reading documents ***
      N += 1
      val doc = iterator.next
      println(N) //println("doc-%d: %s".format(N, doc.name))
      val tokens = Tokenizer.tokenize(doc.content.toLowerCase
        .replaceAll("[,()\"`&/-]", " ").replaceAll("\\s+", " ")) // didn't use the default tokens method of tintyir
      len.update(doc.name, tokens.length) // length of doc
      totalLen += tokens.length
      val tff = TermFrequencies.tf(tokens.filter(allQueryTerms.contains(_))) // only compute tf for tokens in queries --> should be faster... 
      for ((term_wi, freq_wi) <- tff) { // mapping: term wi--> freq of wi
        tf.update(term_wi + "&" + doc.name, freq_wi)
        df.update(term_wi, df.getOrElse(term_wi, 0) + 1)
        cf.update(term_wi, df.getOrElse(term_wi, 0) + freq_wi)
      }
    } // while (iterator.hasNext)

    /* Phase 2. indexing documents, using term-based(tb) and language model (lm)
     * for each query and document, calculate a score `S(q,d)` and rank documents according to S
     */

    /*Term based model*/
    def tfidf(term: String, docname: String) =
      log10(tf.getOrElse(term + "&" + docname, 1) + 1.0) * log10(N * 1.0 / (df.getOrElse(docname, 1) + 1.0))

    def scoreTB(query: Array[String], docname: String) = query.map(tfidf(_, docname)).sum

    /*Language model*/
    def Pwd(term: String, docname: String) = //get the estimated distribution `P(w|d)`
      tf.getOrElse(term + "&" + docname, 0) * 1.0 / len.getOrElse(docname, 1)
    def Pw(term: String) = // get prior distribution of w: `P(w)`
      cf.getOrElse(term, 0) * 1.0 / totalLen
    val lambda = 0.2 // parameter 
    def scoreLM(query: Array[String], docname: String) =
      query.map({ case (term) => (Pwd(term, docname) * (1 - lambda)) + Pw(term) * lambda + 1 })
        .map(log10(_)).sum

    /* Output results*/
    var i = 1
    def neg(t: (Double, String)) = -t._1
    for (query <- queryTerms) {

      val pq = new PriorityQueue[(Double, String)]()(Ordering.by(neg)) //use a minPQ for top 100 docs  
      for (docname <- len.keys) {
        val sTB = scoreTB(query, docname)
        val sLM = scoreLM(query, docname)
        if (pq.size < 100 || pq.head._1 < sTB)
          pq.enqueue((sTB, docname))
        if (pq.size > 100) pq.dequeue()
      } // for doc
      // output result for this query
      var j = 1
      for ((sTB, docname) <- pq.toList.sortBy(_._1*(-1))) {
        println("%d %d %s %f"
          .format(i + 50, j, docname, sTB))
        j += 1
      }
      i += 1
    } // for query

  } //main()

}// object WebIndexing