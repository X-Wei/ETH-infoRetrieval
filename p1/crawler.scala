import java.io.{ File, PrintWriter }
import java.net.URL

import scala.collection.mutable
import scala.io.Source
import scala.util.Properties

object Crawler {

  val visited = mutable.HashSet[String]();

  var urlN = 0;
  var studentN = 0;
  var engPageN = 0;
  var nearDuplicateN = 0;

  var currentURL: String = "";
  var logger = new PrintWriter(new File("log.txt"))

  def binary(value: Int): String = String.format("16%s", Integer.toBinaryString(value)).replace(' ', '0')

  def tokenize(text: String): List[String] = {
    text.split("[ .,;:?!\t\n\r\f]+").toList
  }
  
  def currentPath(curURL: String) = {
    curURL.replaceAll("(?i)([^\\/]+)\\.html", "");
  }
  
  //convert the relative path to absolute path. Using 2 stacks.
  def relative2Absolute(url: String): String = {
    try {
      val reg = "\\.".r
      // if there is a dot in path
      if (reg.findAllIn(url).nonEmpty) {
        val directories = url.split("/").toList
        // a stack of string
        val stack = new mutable.Stack[String]()

        for (direc <- directories) {
          if (direc == ".") {

          } else if (direc == "..") {
            stack.pop()

          } else {
            stack.push(direc);
          }
        }

        val revStack = new mutable.Stack[String]()
        //stack.map(revStack.push(_))
        while (stack.nonEmpty) {
          revStack.push(stack.pop())
        }
        revStack.mkString("/")
      } else {
        url
      }
    } catch {
      case ex: Exception => {
        this.logger.write("ERROR: "+url)
        ""
      }
    }
  } // relative2Absolute()

  def urlProcess(url: String): String = {//  return a valid url
    val reg = "://".r
    if (reg.findAllIn(url).isEmpty) {
      val url_t = currentPath(currentURL) + url
      val c = url_t.replaceAll("\\s+", "")
      //println(c)
      relative2Absolute(c)// convert to absolute path
    } else {
      //println(url)
      relative2Absolute(url).replaceAll("\\s+", "")// remove whitespace
    }
  } // urlProcess()

  //check if the url is in the domain.
  def domainCheck(url: String): Boolean = {

    val constrains = "idvm-infk-hofmann03.inf.ethz.ch/eth/".r
    if (constrains.findAllIn(url).nonEmpty) {
      true
    } else {
      val reg = "://".r
      val urlT = currentPath(url) + url
      //println(urlT)

      if (reg.findAllIn(url).nonEmpty) {
        if (constrains.findAllIn(urlT).nonEmpty) {
          true
        } else {
          false
        }
      } else {
        false
      }
    }

  }
  
  //remove all the tags and some unprintable characters
  def getContent(raw: String): String = {
    raw.replaceAll("<[^>]*>", " ").replaceAll(Properties.lineSeparator, " ").replace("\\t", " ").replaceAll("[\\s]+", " ")
  }

  def getLinks(content: String): List[String] = {
    Properties.lineSeparator
    val aTagRe = "(?s)(?i)<a([^>]+)>(.+?)</a>".r
    //val linkRe = "\\s*(?i)href\\s*=\\s*(\\\"([^\"]*\\\")|'[^']*'|([^'\">\\s]+))".r
    val linkRe = """ \s*(?i)href\s*=\s*\"[^\"]*(/|\.html)\" """.r
    val aTags = aTagRe.findAllIn(content)
    aTags.map(linkRe.findAllIn(_).mkString.replaceAll("href[\\s]*=", "").replaceAll("\"", "")).toList
  }
  
  
  //get the whole html document
  def getRaw(url: String): String = {

    val url_c = new URL(url);
    //println(url)
    try {
      val raw = Source.fromURL(url_c, "ISO-8859-1")
      raw.mkString
    } catch {
      case ex: Exception => {
        this.logger.write("ERROR: "+url)
        ""
      }
    }
  } // getRaw()

  //judge if the page is in English or German if it is in Eng return true else return false.
  def langRec(text: String): Boolean = {
    //val c = new crawl.langRecognizer(0);
    //c.recogEN(text);
    return false
  }
  
  def fingerPrint(content: String){
      val tokens = tokenize(content) 
      val stopwords = Set("the", "a", "is", "it", "are")
      val tokens_wo_stopwords = tokens.filter(!stopwords.contains(_))

      val shingles = tokens.sliding(3).toSet // 3-gram of words
      val hashes = shingles.map(_.hashCode)
      val features = hashes.map(h => binary(h))
      val features_it = features.iterator

      val bitcount = 32
      val table = Array.fill(bitcount)(0)
      while (features_it.hasNext) {
        val feature = features_it.next()
        for (bit <- Range(0, bitcount)) {
          if (feature(bit) == '1') {
            table(bit) += 1
          } else {
            table(bit) -= 1
          }
        }
      }
      val fingerprint = table.mkString
  }
  
  //a bfs crawling strategy
  def crawling(startUrl: String): Unit = {
    val urlQueue = new mutable.Queue[String]; 
    urlQueue.enqueue(startUrl)
    visited.add(startUrl)
    currentURL = ""
    while (urlQueue.nonEmpty) {
      currentURL = urlQueue.dequeue()
      println(currentURL)
      this.logger.write(currentURL+'\n')

      val raw = getRaw(currentURL)
      val content = getContent(raw)
      val regStu = "(?i)student".r //match regardless of capitality
      studentN += regStu.findAllIn(content).size
      if (langRec(content)) {
        engPageN += 1
      }
      val neighborURL = getLinks(raw).map(urlProcess(_)).toSet

      //for all the neighboring urls,if in this domain and not visited, put them in the queue
      val neighborsWhite = neighborURL.filter( 
                  (url: String) => { (!visited.contains(url)) && this.domainCheck(url) } )

      neighborsWhite.map( urlQueue.enqueue(_) )
      neighborsWhite.map( visited.add(_) )



    }// while (queue)
    
    this.urlN = visited.size
  } // crawling()




  //just for testing
  def main(args: Array[String]) = {
    val startURL = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
    crawling(startURL)
    println("Distinct URLs found: " + this.urlN)
    println("Unique English pages found: " + this.engPageN)
    println("Near duplicates found: " + this.nearDuplicateN)
    println("Term frequency of \"student\": " + this.studentN)
  }

} // object Crawler  
