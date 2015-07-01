package kirjainsanapari;

import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class WordPairResult {

  private final String firstWord;
  private final String secondWord;
  private final Set<Character> uniqueLetters;

  public WordPairResult(String firstWord, String secondWord, Set<Character> uniqueLetters) {
    Iterator<String> wordIterator = Stream.of(firstWord, secondWord).sorted().iterator();
    this.firstWord = wordIterator.next();
    this.secondWord = wordIterator.next();
    this.uniqueLetters = uniqueLetters;
  }

  private String lettersToString(Set<Character> letters) {
    List<Character> chars = letters.stream().sorted().collect(Collectors.toList());

    StringBuilder sb = new StringBuilder();
    for (Character character : chars) {
      sb.append(character);
    }

    return sb.toString();
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("{");
    sb.append(firstWord);
    sb.append(", ");
    sb.append(secondWord);
    sb.append("} (");
    sb.append(lettersToString(uniqueLetters));
    sb.append(", ");
    sb.append(uniqueLetters.size());
    sb.append(")");
    return sb.toString();
  }

  @Override
  public boolean equals(Object other) {
    if ((other == null) || !(other instanceof WordPairResult)) {
      return false;
    }

    WordPairResult otherResult = (WordPairResult)other;

    if (!otherResult.firstWord.equalsIgnoreCase(firstWord)) {
      return false;
    }
    if (!otherResult.secondWord.equalsIgnoreCase(secondWord)) {
      return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    return 31 * firstWord.hashCode() + 59 * secondWord.hashCode();
  }
}
