<TeXmacs|1.99.2>

<style|generic>

<\body>
  <section|system>

  All code is in the file <verbatim|WebIndexing.scala>, this scala code runs
  in 4 steps (seperated by comments in the code), the 4 steps are as
  described below:

  <subsection|Preparing candidate terms from queries>

  First we aggregate all queries, tokenize them and (after stopwording) we
  get a list of unique terms in all queries. This list of query terms is
  about 100 in size.\ 

  <subsection|Reading through documents and getting statistics>

  The second phase of the programme consist in passing through all documents,
  and for each document <with|font-shape|italic|d>, we calculate the term
  frequency of this document and update our statistics.\ 

  The statics include:

  <\itemize>
    <item><verbatim|tf(w,d)>: the term frequency, notice we are only
    interested in terms <verbatim|w> <with|font-shape|italic|in query terms>,
    in scala this is represented by a <verbatim|HashMap[(String, String),
    Int]>

    <item><verbatim|df(w)>: document frequency for term <verbatim|w>
    <with|font-shape|italic|(in query terms)>, <verbatim|HashMap[String,
    Int]> in scala

    <item><verbatim|cf(w)>: collection frequency \ for term <verbatim|w>
    <with|font-shape|italic|(in query terms)>, \ <verbatim|HashMap[String,
    Int]> in scala

    <item><verbatim|len(d)>: mapping from document to its length,
    \ <verbatim|HashMap[String, Int]> in scala

    <item><verbatim|N>: number of document in total

    <item><verbatim|totalLen>: length of total document length
  </itemize>

  We will store there statistics in memory so that we can performe
  calcluations later on.\ 

  <subsection|Indexing of documents>

  In this phase we do the indexing.\ 

  For each query, we go over all documents again (but this time only to look
  up hashmaps in memory instead of going through zip files), and calculate
  the term-based score and language-model score according to the equations
  described above. Then we maintain minimum priority queues of maixmum size
  100 to store the revalant documents, so that at last we get 100
  highest-scored documents.\ 

  We keep these lists in memeory for evaluation later on. Also in this step
  we have outputed the results to files.\ 

  <subsection|Evaluation>

  In this step we do the evaluation.\ 

  We first read the content in <verbatim|qrel.txt>, and construct the ground
  truth hashmap.\ 

  For each query we then go over our result, compare it with the ground
  truth, and calculate the evaluation mertics.\ 

  \;

  <section|Training performance>

  <big-table|<block|<tformat|<table|<row|<cell|>|<cell|precision>|<cell|recall>|<cell|MAP>>|<row|<cell|term-based>|<cell|0.134750>|<cell|0.058697>|<cell|0.067742>>|<row|<cell|language-model>|<code|>0.087750|<cell|0.047216>|<cell|0.035881>>>>>|Performance
  of our models on training data>
</body>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1.2|?>>
    <associate|auto-4|<tuple|1.3|?>>
    <associate|auto-5|<tuple|1.4|?>>
    <associate|auto-6|<tuple|2|?>>
    <associate|auto-7|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>system>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>