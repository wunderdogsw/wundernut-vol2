package kirjainsanapari;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class UniqueLetterWordPairFinder {
  private static final Comparator<Set<Character>> CHARACTER_COMBINATION_COMPARATOR = new Comparator<Set<Character>>() {
    
    @Override
    public int compare(Set<Character> characterSet1, Set<Character> characterSet2) {
      return characterSet2.size() - characterSet1.size();
    }
  };

  private Map<Set<Character>, Set<String>> characterCombinationsToWords = new HashMap<Set<Character>, Set<String>>();

  private List<Set<Character>> getUniqueCharacterCombinationsFromFile(String filename) {
    characterCombinationsToWords = new HashMap<>();
    
    Set<Set<Character>> characterCombinations = new HashSet<>();
    try {
      Path path = FileSystems.getDefault().getPath(filename);
      List<String> allLines = Files.readAllLines(path);
      for (String line : allLines) {
        String[] words = line.split("\\s");
        for (String word : words) {
          String properWord = removeNonWordCharacters(word);
          if (!properWord.isEmpty()) {
            Set<Character> uniqueCharacters = UniqueLetterFinder.uniqueLetters(properWord);

            Set<String> wordsWithCharacters = characterCombinationsToWords.get(uniqueCharacters);
            if (wordsWithCharacters == null) {
              wordsWithCharacters = new HashSet<>();
            }
            wordsWithCharacters.add(properWord);

            characterCombinations.add(uniqueCharacters);
            
            characterCombinationsToWords.put(uniqueCharacters, wordsWithCharacters);
          }
        }
      }
    } catch (IOException ioe) {
      System.err.println("Ongelma tiedoston luvussa: " + ioe.getMessage());
    }

    List<Set<Character>> orderedCharacterCombinations = new ArrayList<Set<Character>>(characterCombinations);
    orderedCharacterCombinations.sort(CHARACTER_COMBINATION_COMPARATOR);
    return orderedCharacterCombinations;
  }

  private String removeNonWordCharacters(String word) {
    return word.replaceAll("[" + UniqueLetterFinder.NON_WORD_CHARACTERS + "]", "");
  }

  private Set<CombinationPairResult> getPairsWithMostUniqueLetters(List<Set<Character>> allCharacterCombinations) {
    Set<CombinationPairResult> results = new HashSet<>();
    List<Set<Character>> wordsToCheck = new ArrayList<>(allCharacterCombinations);
    int bestScore = 0;

    outer:
    while (wordsToCheck.size() >= 2) {
      Set<Character> firstWord = wordsToCheck.remove(0);
      for (Set<Character> secondWord : wordsToCheck) {
        if (secondWord.size() < (bestScore - firstWord.size())) {
          continue outer;
        }
        Set<Character> uniqueCharactersInPair = new HashSet<>(firstWord);
        uniqueCharactersInPair.addAll(secondWord);
        
        int score = uniqueCharactersInPair.size();
        if (score >= bestScore) {
          if (score > bestScore) {
            bestScore = score;
            results.clear();
          }

          results.add(new CombinationPairResult(firstWord, secondWord, uniqueCharactersInPair));
        }
      }
    }

    return results;
  }

  public Collection<WordPairResult> findWordPairsWithMostUniqueLettersFromFile(String filename) {
    long start = System.currentTimeMillis();
    List<Set<Character>> allCharacterCombinations = getUniqueCharacterCombinationsFromFile(filename);
    Set<CombinationPairResult> combinationPairs = getPairsWithMostUniqueLetters(allCharacterCombinations);
    
    List<WordPairResult> wordPairResult = new ArrayList<>();
    for (CombinationPairResult combinationPairResult : combinationPairs) {
      Set<Character> firstCombination = combinationPairResult.getFirstCombination();
      Set<Character> secondCombination = combinationPairResult.getSecondCombination();
      
      Set<String> wordsForFirstCombination = characterCombinationsToWords.get(firstCombination);
      for (String firstWord : wordsForFirstCombination) {
        Set<String> wordsForSecondCombination = characterCombinationsToWords.get(secondCombination);
        for (String secondWord : wordsForSecondCombination) {
          wordPairResult.add(new WordPairResult(firstWord, secondWord, combinationPairResult.getUniqueLetters()));
        }
      }
    }
    long end = System.currentTimeMillis();

    String report = String.format("Found %d results in %d ms.", combinationPairs.size(), (end - start));
    System.out.println(report);

    return wordPairResult;
  }

  public static void main(String[] args) {
    UniqueLetterWordPairFinder finder = new UniqueLetterWordPairFinder();
    System.out.println(finder.findWordPairsWithMostUniqueLettersFromFile("alastalo.txt"));
  }
}
