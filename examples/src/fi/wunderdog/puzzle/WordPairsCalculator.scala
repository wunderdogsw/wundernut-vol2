package fi.wunderdog.puzzle

object WordPairsCalculator extends App {

  type CharsAndWords = (String, List[String])
  type CharsAndWordsMap = Map[String, List[String]]

  val startTime = System.currentTimeMillis()
  val fileContent = scala.io.Source.fromFile("alastalon_salissa.txt").mkString

  val distinctWords = fileContent.replaceAll("[\\r\\n]", " ").toLowerCase().replaceAll("[^a-zöäå\\-\\s]", "").split(" ").distinct.toList
  val distinctWordsAndDistinctChars = distinctWords.map(word => (word -> getDistinctChars(word).toList.sorted.mkString))
  val wordsByDistinctChars = distinctWordsAndDistinctChars.groupBy(pair => pair._2).map(mapped => mapped._1 -> mapped._2.map(word => word._1))

  val greatestLengthOfDistinctCharsInSingleWord = wordsByDistinctChars.map(chars => chars._1.length()).max
  val wordOfGreatestDistinctCharsLength = wordsByDistinctChars.filter(v => v._1.length() == greatestLengthOfDistinctCharsInSingleWord).keys.head

  val pairsForSingleMaxWord = wordsByDistinctChars.map { charsAndWords =>
    charsAndWords._1.replaceAll(charFilter(wordOfGreatestDistinctCharsLength), "")
  }
  val maxLengthOfMaxPairOfGreatestWord = pairsForSingleMaxWord.map(s => s.length()).max
  val referenceLength = greatestLengthOfDistinctCharsInSingleWord + maxLengthOfMaxPairOfGreatestWord

  val wordSetsRange = (maxLengthOfMaxPairOfGreatestWord to greatestLengthOfDistinctCharsInSingleWord)

  val wordSets = for {
    rangeCount <- wordSetsRange
  } yield (wordsByDistinctChars.filter(_._1.length() == rangeCount), rangeCount)

  val wordSetsFromMaxToMin = wordSets.reverse
  val numOfSetsToProcess = wordSetsFromMaxToMin.length / 2 + 1
  val wordSetsFromMaxToMinToProcess = wordSetsFromMaxToMin.take(numOfSetsToProcess)

  val greatestWordPairsWithDistinctChars = wordSetsFromMaxToMinToProcess.par.map { currentWordSet =>
    val initialWordSetsToCount = wordSetsFromMaxToMin.filter(wordSet => currentWordSet._2 + wordSet._2 >= referenceLength)
    val wordSetsToCount = initialWordSetsToCount.filter(wordSet => wordSet._2 <= currentWordSet._2)
    getMaxForSet(currentWordSet, wordSetsToCount).flatten
  }.flatten

  def getMaxForSet(currentSet: (CharsAndWordsMap, Int), wordSetsToCount: Seq[(CharsAndWordsMap, Int)]): Iterable[Seq[CharsAndWords]] = {
     println(s"${System.currentTimeMillis()-startTime} ms: evaluating words for ${currentSet._2} (${currentSet._1.size}) against ${wordSetsToCount.map(s => s._2)}")

     def numberOfDistinctChars(firstWord: String, secondWord: String) = {
       val allChars = getDistinctChars(firstWord + secondWord).sorted
       allChars.length()
     }

     def composeDistinctCharsAndWordsTuple(firstWord: CharsAndWords, secondWord: CharsAndWords): CharsAndWords = {
       val allDistinctChars = getDistinctChars(firstWord._1 + secondWord._1).sorted
       (allDistinctChars, firstWord._2 ++ secondWord._2)
     }

     currentSet._1.map { charsAndWords =>
       wordSetsToCount.flatMap { wordSet =>
         wordSet._1
           .filter(word => numberOfDistinctChars(charsAndWords._1, word._1) >= referenceLength)
           .map(word => composeDistinctCharsAndWordsTuple(charsAndWords, word))
       }
    }
  }

  def getDistinctChars(word: String): String = word.toLowerCase().replaceAll("[^a-zöäå]", "").distinct

  def charFilter(chars: String): String = s"[${chars.trim}]"

  val maxDistinctCharsInPairs = greatestWordPairsWithDistinctChars.map(charsAndWords => charsAndWords._1.length()).max
  val wordsWithMaxDistinctChars = greatestWordPairsWithDistinctChars.filter(cw => cw._1.length() == maxDistinctCharsInPairs)
  val distinctPairs = wordsWithMaxDistinctChars.map(pairs => pairs._1 -> pairs._2.toList.sorted).toSet

  println(s"Total of ${distinctPairs.size} word pairs with sum of ${maxDistinctCharsInPairs} characters")

  distinctPairs.map{pair =>
    val chars = pair._1
    val words = pair._2.mkString(" & ")
    println(s"[$chars] $words")
  }

  println(s"${System.currentTimeMillis() - startTime} ms: Total execution time: ")
}
