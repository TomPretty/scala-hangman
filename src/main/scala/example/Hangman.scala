package example

import Utils._

case class GameState(
    word: String,
    guesses: Set[Char] = Set()
) {
  def numWrongGuesses = {
    guesses.toSeq.filter(!word.contains(_)).length
  }
}

object Hangman extends App {
  mainLoop(GameState(word = "hello"))

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
          val newState = state.copy(guesses=state.guesses + guess)

          if (hasGuessedWord(newState)) {
            printGameSummary(newState)
            printGuessedWordMessage(state.word)
          } else if (hasRanOutOfGuesses(newState)) {
            printGameSummary(newState)
            printRanOutOfGuessesMessage(state.word)
          } else {
            mainLoop(newState)
          }
        }
      }
    }
  }

  def isDuplicateGuess(guess: Char, guesses: Set[Char]) = {
    guesses.contains(guess)
  }

  def hasGuessedWord(state: GameState) = {
    state.word.toCharArray.forall(state.guesses.contains(_))
  }

  def hasRanOutOfGuesses(state: GameState) = {
    state.numWrongGuesses >= 7
  }
}
