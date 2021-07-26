package example

import Utils._

case class GameState(
    word: List[Letter],
    guesses: Set[Letter] = Set()
) {
  def wordAsString = word.map(Letter.toChar).mkString("")

  def numWrongGuesses = {
    guesses.toSeq.filter(!word.contains(_)).length
  }
}

object Hangman extends App {
  mainLoop(GameState(word = List(H, E, L, L, O)))

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
            printGuessedWordMessage(state)
          } else if (hasRanOutOfGuesses(newState)) {
            printGameSummary(newState)
            printRanOutOfGuessesMessage(state)
          } else {
            mainLoop(newState)
          }
        }
      }
    }
  }

  def isDuplicateGuess(guess: Letter, guesses: Set[Letter]) = {
    guesses.contains(guess)
  }

  def hasGuessedWord(state: GameState) = {
    state.word.forall(state.guesses.contains(_))
  }

  def hasRanOutOfGuesses(state: GameState) = {
    state.numWrongGuesses >= 7
  }
}
