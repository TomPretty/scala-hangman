package example

import Utils._

case class GameState(
    word: String,
    guesses: List[Char]
) {
  def numWrongGuesses = {
    guesses.filter(!word.contains(_)).length
  }
}

object Hangman extends App {
  mainLoop(GameState(word = "hello", guesses = Nil))

  def mainLoop(state: GameState): Unit = {
    printGameSummary(state)
    showPrompt
    readInput match {
      case None => {
        printInvalidInputMessage
        mainLoop(state)
      }
      case Some(guess) => {
        if (isDuplicateGuess(guess, state.guesses)) {
          printDuplicateGuessMessage(guess)
          mainLoop(state)
        } else {
          val newGuesses = guess +: state.guesses
          val newState = GameState(state.word, newGuesses)

          if (hasGuessedWord(newState)) {
            printGameSummary(newState)
            printGuessedWordMessage(state.word)
          } else if (hasRanOutOfGuesses(newState)) {
            printGameSummary(newState)
            printRanOutOfGuessesMessage(state.word)
          } else {
            mainLoop(GameState(state.word, newGuesses))
          }
        }
      }
    }
  }

  def isDuplicateGuess(guess: Char, guesses: List[Char]) = {
    guesses.contains(guess)
  }

  def hasGuessedWord(state: GameState) = {
    state.word.toCharArray.forall(state.guesses.contains(_))
  }

  def hasRanOutOfGuesses(state: GameState) = {
    state.numWrongGuesses >= 7
  }
}
