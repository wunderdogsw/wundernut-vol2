package wunderdog

import scala.io.Source
import scala.collection.mutable.Set

/**
 * @author Ville Komulainen, https://github.com/vkomulai
 * Almost brute force solution to Wunderdog coding challenge #2.
 * Naive and far from optimal, but does the trick in reasonable time with relatively simple code.
 * Processes src/test/resources/alastalon_salissa.txt in ~14 secs on Intel(R) Core(TM) i7-4850HQ CPU @ 2.30GHz. YMMV.
 */
case class WordData(word: String, distinctAlphabets: List[Char])
case class WordPair(first: String, second: String, distinctAlphabetCount: Int)
object WordCombo extends App {

  val WORD_SEPARATORS = "[\\s+,\\.\\?\\:\\'\"\\!\\-]"

  def orderedPairFrom(word1: String, word2: String) = {
    if (word1 <= word2) (word1, word2) else (word2, word1)
  }

  def isTheoreticallyNewMaxPair(word1: WordData, word2: WordData, maxPair: Set[WordPair]) = {
    maxPair.isEmpty || word2.distinctAlphabets.length + word1.distinctAlphabets.length >= maxPair.head.distinctAlphabetCount
  }

  val wordsFile = args.length match {
    case 0 => "src/test/resources/alastalon_salissa.txt"
    case _ => args(0)
  }

  /** Words in descending distinct alphabet order */
  val distinctWords = Source.fromFile(wordsFile)
                            .getLines
                            .flatMap(_.toLowerCase().split(WORD_SEPARATORS))
                            .toList
                            .distinct
                            .map(word => WordData(word, word.toList.distinct))
                            .sortBy(-1 * _.distinctAlphabets.length)

  val maxWordPairs = Set.empty[WordPair]
  distinctWords.foreach(word1 =>  {
    /** BF only such combinations which in theory can contain more alphabets than the current max. */
    distinctWords.takeWhile(word2 => isTheoreticallyNewMaxPair(word1, word2, maxWordPairs))
                 .foreach(word2 => {
                    val orderedPair = orderedPairFrom(word1.word, word2.word)
                    val wordPair = WordPair(orderedPair._1,
                                            orderedPair._2,
                                            (word1.distinctAlphabets ::: word2.distinctAlphabets).distinct.length)
                    if (maxWordPairs.isEmpty) {
                      maxWordPairs += wordPair
                    } else if (wordPair.distinctAlphabetCount > maxWordPairs.head.distinctAlphabetCount) {
                      maxWordPairs.clear
                      maxWordPairs += wordPair
                    } else if (wordPair.distinctAlphabetCount == maxWordPairs.head.distinctAlphabetCount) {
                      maxWordPairs += wordPair
                    }
                })
              })

  maxWordPairs.foreach(println)
}
