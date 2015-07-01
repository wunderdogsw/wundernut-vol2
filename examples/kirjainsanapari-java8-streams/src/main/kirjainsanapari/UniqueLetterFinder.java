package kirjainsanapari;

import java.util.Set;
import java.util.stream.Collectors;

public class UniqueLetterFinder {

  public static final String NON_WORD_CHARACTERS = ",.:;\"'!?()";
  public static final String NON_COUNTABLE_CHARACTERS = NON_WORD_CHARACTERS + "-[\\d]";

  public static Set<Character> uniqueLetters(String word) {
    Set<Character> uniqueCharacters = word.replaceAll("[" + NON_COUNTABLE_CHARACTERS + "]", "")
                                          .toUpperCase()
                                          .chars()
//                                          .distinct()
                                          .mapToObj(i -> (char)i)
                                          .collect(Collectors.toSet());
    return uniqueCharacters;
  }
}
