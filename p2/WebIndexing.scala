import java.io.{ BufferedWriter, File, FileWriter }
import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.dal.tinyir.processing.{ TipsterCorpusIterator, Tokenizer }
import scala.io.Source
import scala.math._
import scala.collection.mutable.{ HashMap, PriorityQueue, HashSet }
import scala.math.Ordering.Implicits._
import scala.collection.mutable.ListBuffer

object WebIndexing {

  def main(args: Array[String]): Unit = {
    /* Phase 0. Prepare queries
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

    /* Phase 1. Reading documents
     * To apply term-based model/language model, we need:
     * tf(w,d) --HashMap[String, Int] (instead of `HashMap[Array[String], Int]`)
     * df(w) --HashMap[String, Int]
     * cf(w) --HashMap[String, Int]
     * len(d) := length of document d --HashMap[String, Int] 
     * N := nb of documents
     * totalLen := length of all docs (sum of cf(v) for all v)
     */
    val tf = new HashMap[String, Int] // use 'term&docname' as the key
    val df = new HashMap[String, Int]
    val cf = new HashMap[String, Int]
    val len = new HashMap[String, Int]
    var N = 0
    var totalLen = 0

    val iterator = new TipsterCorpusIterator("../zips/zips-small")
    while (iterator.hasNext) { //*** main loop for reading documents ***
      N += 1
      val doc = iterator.next
      println("doc-%d: %s".format(N, doc.name)) //println(N)
      val tokens = Tokenizer.tokenize(doc.content.toLowerCase
        .replaceAll("[,()\"`&/-]", " ").replaceAll("\\s+", " ")) // didn't use the default tokens method of tintyir
      len.update(doc.name, tokens.length) // length of doc
      totalLen += tokens.length
      val tff = TermFrequencies.tf(tokens.filter(allQueryTerms.contains(_))) // only compute tf for tokens in queries --> should be faster... 
      for ((term_wi, freq_wi) <- tff) { // mapping: term wi--> freq of wi
        tf.update(term_wi + "&" + doc.name.replaceAll("-", ""), freq_wi)
        df.update(term_wi, df.getOrElse(term_wi, 0) + 1)
        cf.update(term_wi, df.getOrElse(term_wi, 0) + freq_wi)
      }
    } // while (iterator.hasNext)

    /* Phase 2. Indexing documents, using term-based(tb) and language model (lm)
     * for each query and document, calculate a score `S(q,d)` and rank documents according to S
     */

    /* Term based model*/
    def tfidf(term: String, docname: String) =
      log10(tf.getOrElse(term + "&" + docname, 1) + 1.0) * log10(N * 1.0 / (df.getOrElse(docname, 1) + 1.0))

    def scoreTB(query: Array[String], docname: String) = query.map(tfidf(_, docname)).sum

    /* Language model*/
    def Pwd(term: String, docname: String) = //get the estimated distribution `P(w|d)`
      tf.getOrElse(term + "&" + docname, 0) * 1.0 / len.getOrElse(docname, 1)
    def Pw(term: String) = // get prior distribution of w: `P(w)`
      cf.getOrElse(term, 0) * 1.0 / totalLen
    val lambda = 0.2 // parameter 
    def scoreLM(query: Array[String], docname: String) =
      query.map({ case (term) => (Pwd(term, docname) * (1 - lambda)) + Pw(term) * lambda + 1 })
        .map(log10(_)).sum

    /* Output results*/
    val file_tb = new File("result_tb.txt")
    val bw_tb = new BufferedWriter(new FileWriter(file_tb))
    val file_lm = new File("result_lm.txt")
    val bw_lm = new BufferedWriter(new FileWriter(file_lm))

    val resTB = new HashMap[Int, ListBuffer[String]] // mapping from queryId to list of documents
    val resLM = new HashMap[Int, ListBuffer[String]]

    def neg(t: (Double, String)) = -t._1
    var i = 51
    for (query <- queryTerms) {
      println("processing query-%d".format(i))
      val pqTB = new PriorityQueue[(Double, String)]()(Ordering.by(neg)) //use a minPQ for top 100 docs  
      val pqLM = new PriorityQueue[(Double, String)]()(Ordering.by(neg))
      for (docname <- len.keys) {
        val sTB = scoreTB(query, docname)
        if (pqTB.size < 100 || pqTB.head._1 < sTB)
          pqTB.enqueue((sTB, docname))
        if (pqLM.size > 100) pqTB.dequeue()
        val sLM = scoreLM(query, docname)
        if (pqLM.size < 100 || pqLM.head._1 < sLM)
          pqTB.enqueue((sLM, docname))
        if (pqTB.size > 100) pqTB.dequeue()
      } // for doc
      // output result for this query
      var j = 1
      for ((sTB, docname) <- pqTB.toList.sortBy(_._1 * (-1))) {
        resTB.getOrElseUpdate(i, new ListBuffer[String]()) += docname // append docname to the result list
        //        print("%d %d %s %f\n".format(i, j, docname, sTB))
        bw_tb.write("%d %d %s %f\n".format(i + 50, j, docname, sTB))
        j += 1
      }
      j = 1
      for ((sTB, docname) <- pqTB.toList.sortBy(_._1 * (-1))) {
        resLM.getOrElseUpdate(i, new ListBuffer[String]()) += docname
        bw_lm.write("%d %d %s %f\n".format(i, j, docname, sTB))
        j += 1
      }
      i += 1
    } // for query
    bw_lm.close()
    bw_tb.close()

    /* Phase 3. Evaluate
     * using `resTB` and `resLM` and compare with content in qrel.txt
     * output performance to console and file
     */
    val trainQrels = Source.fromFile("qrels").getLines
      .filter(_.endsWith("1"))
      .map(_.replaceAll("-", "").split(" ").toList)
      .toList // List[List[String]]
    val training = new HashMap[Int, HashSet[String]] // for evaluation: mapping from queryId to set of revlant docs
    for (res <- trainQrels) {
      val queryId = res(0)
      val docname = res(2)
      training.getOrElseUpdate(Integer.parseInt(queryId), new HashSet[String])
        .add(docname)
    }
    // for each query, compare our result to the truth
    var sumAP, sumP, sumR = 0.0
    for (queryId <- 51 to 90) {
      var tp, fp, fn = 0
      var precision, recall, AP = 0.0
      val lst = resTB.getOrElse(queryId, new ListBuffer[String]()) // list of revalant documents
      val truth = training.getOrElse(queryId, new HashSet[String]) // hashset of revlant dos
      var count = 1
      for (docname <- lst) {
        if (truth.contains(docname)) {
          tp += 1
          precision = tp * 1.0 / count
          AP += precision //recall = tp * 1.0 / truth.size
        }
        count += 1
      }
      fn = truth.size - tp
      fp = 100 - tp
      precision = tp * 1.0 / (tp + fp) //2B..
      recall = tp * 1.0 / (tp + fn)
      AP = AP / (tp + fn)
      sumAP += AP
      sumP += precision
      sumR += recall
      println("query %d, precision = %f, recall = %f, AP = %f".format(queryId, precision, recall, AP))
    } // for queryid = 51 to 90
    println("MAP = %f, mean precision = %f, mean recall = %f".format(sumAP / 40, sumP / 40, sumR / 40))
  } //main()

}// object WebIndexing