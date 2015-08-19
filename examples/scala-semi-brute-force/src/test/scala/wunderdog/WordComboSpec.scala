package wunderdog

import java.io.{ByteArrayOutputStream, PrintStream}

import org.scalatest.FlatSpec

/**
 * @author Ville Komulainen, https://github.com/vkomulai
 */
class WordComboSpec extends FlatSpec {

  "WordCombo" should "print two WordPairs of length 9" in {
      // Hack to obtain printed words from stdout
      val stdoutStream = new ByteArrayOutputStream
      val stdoutRedirectStream = new PrintStream(stdoutStream)

      Console.withOut(stdoutRedirectStream)(WordCombo.main(Array("src/test/resources/test_data.txt")))
      val outPut = stdoutStream.toString("UTF-8")
      val wordCombos = outPut.split(System.lineSeparator).toList

      assert(wordCombos.length == 2)
      assert(wordCombos.contains("WordPair(cat,kinder,9)"))
      assert(wordCombos.contains("WordPair(cat,wunder,9)"))
    }
}
