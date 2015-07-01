package kirjainsanapari;

import static org.junit.Assert.assertEquals;

import java.util.Collection;

import org.junit.Test;

public class UniqueLetterWordPairFinderTest {

  private static final String TEST_FILE_PATH = "resources/test/";

  @Test
  public void tsuikkisSentenceShouldHaveOneBestAnswer() {
    Collection<WordPairResult> res = getResultsForFile("ruoka-ainevirke.txt");
    assertEquals(1, res.size());
  }

  @Test
  public void kukkoSentenceShouldHaveFourBestAnswers() {
    Collection<WordPairResult> res = getResultsForFile("ideavirke.txt");
    assertEquals(4, res.size());
  }

  @Test
  public void repeatingWordsShouldOnlyBeCountedOnce() {
    Collection<WordPairResult> res = getResultsForFile("words_repeating.txt");
    assertEquals(1, res.size());
  }

  private Collection<WordPairResult> getResultsForFile(String filename) {
    UniqueLetterWordPairFinder finder = new UniqueLetterWordPairFinder();
    return finder.findWordPairsWithMostUniqueLettersFromFile(TEST_FILE_PATH + filename);
  }
}
