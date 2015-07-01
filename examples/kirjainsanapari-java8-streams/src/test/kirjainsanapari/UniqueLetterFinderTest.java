package kirjainsanapari;

import static org.junit.Assert.assertEquals;

import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

public class UniqueLetterFinderTest {

  @Test
  public void findingUniqueLettersFromSeveralInstancesOfOneLetterShouldReturnOneLetter() {
    assertEquals(createExpectationOf("A"), UniqueLetterFinder.uniqueLetters("aaa"));
  }

  @Test
  public void findingUniqueLettersFromDifferentLettersShouldReturnOneOfEach() {
    assertEquals(createExpectationOf("ABCDE"), UniqueLetterFinder.uniqueLetters("abcde"));
  }

  @Test
  public void findingUniqueLettersShouldOrderResults() {
    assertEquals(createExpectationOf("ABCÅÄÖ"), UniqueLetterFinder.uniqueLetters("ÖÄÅCBA"));
  }

  @Test
  public void findingUniqueLettersShouldIgnoreCase() {
    assertEquals(createExpectationOf("A"), UniqueLetterFinder.uniqueLetters("aA"));
  }

  @Test
  public void findingUniqueLettersShouldIgnoreSpecialCharacters() {
    assertEquals(createExpectationOf("ABCDEFGHIJKL"), UniqueLetterFinder.uniqueLetters("a-b:c.d,e;f\"g'h!i?j(k)l"));
  }

  @Test
  public void findingUniqueLettersShouldIgnoreNumbers() {
    assertEquals(createExpectationOf("AB"), UniqueLetterFinder.uniqueLetters("a123b456"));
  }

  private Set<Character> createExpectationOf(String characters) {
    return characters.chars().mapToObj(c -> (char)c).collect(Collectors.toSet());
  }
}
