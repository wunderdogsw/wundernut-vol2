package kirjainsanapari;

import java.util.Set;

public class CombinationPairResult {

  private final Set<Character> firstCombination;
  private final Set<Character> secondCombination;
  private final Set<Character> uniqueLetters;

  public Set<Character> getFirstCombination() {
    return firstCombination;
  }

  public Set<Character> getSecondCombination() {
    return secondCombination;
  }

  public Set<Character> getUniqueLetters() {
    return uniqueLetters;
  }

  public CombinationPairResult(Set<Character> firstCombination, Set<Character> secondCombination, Set<Character> uniqueLetters) {
    this.firstCombination = firstCombination;
    this.secondCombination = secondCombination;
    this.uniqueLetters = uniqueLetters;
  }

  @Override
  public boolean equals(Object other) {
    if ((other == null) || !(other instanceof CombinationPairResult)) {
      return false;
    }

    CombinationPairResult otherResult = (CombinationPairResult)other;

    if ((otherResult.firstCombination.equals(firstCombination) && otherResult.secondCombination.equals(secondCombination))
        || (otherResult.secondCombination.equals(firstCombination) && otherResult.firstCombination.equals(secondCombination))) {
      return true;
    }

    return false;
  }

  @Override
  public int hashCode() {
    return 31 * firstCombination.hashCode() + 59 * secondCombination.hashCode();
  }
}
