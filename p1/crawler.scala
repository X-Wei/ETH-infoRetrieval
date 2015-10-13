import java.io.{ File, PrintWriter }
import java.net.URL
import java.security.MessageDigest
import scala.collection.mutable
import scala.io.Source
import scala.util.Properties

object Crawler {

  val visited = mutable.HashSet[String]();
  //val visited_fps = mutable.HashMap[String,String]();
  val visited_fps = mutable.HashMap[Int, String]();

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
      val reg = """/\.""".r
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
        this.logger.write("ERROR: " + url)
        ""
      }
    }
  } // relative2Absolute()

  def urlProcess(url: String): String = { //  return a valid url
    val reg = "://".r
    if (reg.findAllIn(url).isEmpty) {
      val url_t = currentPath(currentURL) + url
      val c = url_t.replaceAll("\\s+", "")
      //println(c)
      relative2Absolute(c) // convert to absolute path
    } else {
      //println(url)
      relative2Absolute(url).replaceAll("\\s+", "") // remove whitespace
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
    val linkRe = """(?i)(href\s*=\s*[\"\']?[^\"]*\.html[\'\"]?)""".r
    val aTags = aTagRe.findAllIn(content)
    aTags.map(linkRe.findAllIn(_).mkString.replaceAll("href[\\s]*=", "")
      .replaceAll("""[\?|\#].*""", "") // "disregard any anchors (#) or get parameters (?)"
      .replaceAll("\"", "")).toList
  }

  //get the whole html document
  def getRaw(url: String): String = {

    val url_c = new URL(url);
    try {
      val raw = Source.fromURL(url_c, "ISO-8859-1")
      raw.mkString
    } catch {
      case ex: Exception => {
        this.logger.write("ERROR: " + url)
        "" //if exception: return empty string
      }
    }
  } // getRaw()

  //judge if the page is in English or German if it is in Eng return true else return false.
  def langRec(text: String): Boolean = {
    //val c = new crawl.langRecognizer(0);
    //c.recogEN(text);
    return false
  }

  def MD5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  } // return MD5 value in a byte array (16 bytes, 128 bits)

  def simHash128(content: String): String = {
    val tokens = tokenize(content)
    val shingles = tokens.sliding(3).toSet.toArray // 3-gram of words
    val hashes = shingles.map(s => MD5(s.mkString)) // each shingle --> hashcode of 128bits, in a byte array 
    val fp = Array[Byte](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val MASK = 0xF // 1111
    val sz = hashes.size
    for (i <- 0 to 127) {
      val index = i / 8
      val j = i & MASK;
      if (hashes.map(h => (h(index) & (1 << j)) >> j).sum > sz / 2.0)
        fp(index) = (fp(index) | (1 << j)).toByte
    }
    return fp.map("%3d".format(_).replace(' ', '0')).mkString
  }

  def simHash32(content: String): Int = {
    val tokens = tokenize(content)
    val shingles = tokens.sliding(3).toSet.toArray // 3-gram of words
    val hashes = shingles.map(_.hashCode).toArray // each shingle --> hashcode of 32bits 
    var fp: Int = 0
    val sz = hashes.size
    for (i <- 0 to 31) {
      if (hashes.map(h => (h & (1 << i)) >> i).sum > sz / 2.0)
        fp |= (1 << i)
    }
    return fp
  }

  def isNearDup(hs: Int): Boolean = { //test if a hashcode is a near duplicate
    for (hash <- visited_fps.keySet) {
      if (bit_diff(hash, hs) <= 2)
        return true;
    }
    return false
  }

  def bit_diff(hash: Int, hs: Int) = {
    var count = 0;
    for (i <- 0 to 31) {
      val mask = 1 << i
      if (((hash ^ hs) & mask) > 0) count += 1;
    }
    count
  }

  //a bfs crawling strategy
  def crawling(startUrl: String): Unit = {
    val urlQueue = new mutable.Queue[String];
    urlQueue.enqueue(startUrl)
    visited.add(startUrl)
    currentURL = ""
    while (urlQueue.nonEmpty) {
      currentURL = urlQueue.dequeue()

      val raw = getRaw(currentURL)
      val content = getContent(raw)

      if (content.length() > 0) { //avoid invalid urls // && !raw.contains("redirected")
        println(currentURL + ", " + content.length())
        this.logger.write(currentURL + ", " + content.length() + '\n')
        this.urlN += 1; // only valid urls count
        val regStu = "(?i)student".r //match regardless of capitality
        val fp = simHash32(content)
        if (isNearDup(fp)){//(visited_fps.contains(fp)) {
          this.nearDuplicateN += 1
          println("found dup:" + visited_fps.get(fp) + ", " + currentURL + ", " + content.length())
          logger.write("found dup:" + visited_fps.get(fp) + ", " + currentURL + ", " + content.length() + '\n')
        }
        visited_fps(fp) = currentURL
        studentN += regStu.findAllIn(content).size
        if (langRec(content)) {
          engPageN += 1
        }
        val neighborURL = getLinks(raw).map(urlProcess(_)).toSet

        //for all the neighboring urls,if in this domain and not visited, put them in the queue
        val neighborsWhite = neighborURL.filter(
          (url: String) => { (!visited.contains(url)) && this.domainCheck(url) && url.endsWith("html") })

        neighborsWhite.map(urlQueue.enqueue(_))
        neighborsWhite.map(visited.add(_))
      }

    } // while (queue)

    //this.urlN = visited.size
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