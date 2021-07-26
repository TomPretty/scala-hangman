package example

import scala.io.StdIn.readLine

object Utils {
  def showPrompt: Unit = print("What's your guess? ")

  def readInput: Option[Char] = {
    val input = readLine()
    if (input.length > 1) {
      None
    } else {
      input.headOption.flatMap(c =>
        if (c.isLetter) Some(c.toLower)
        else None
      )
    }
  }

  def readLetter: Option[Letter] = {
    readChar.flatMap(Letter.fromChar(_))
  }

  def readChar: Option[Char] = {
    val input = readLine()

    if (input.length == 1) Some(input(0)) else None
  }

  def printGameSummary(state: GameState): Unit = {
    val word = state.word.toCharArray
      .map(c => if (state.guesses.contains(c)) c else '_')
      .mkString(" ")
    val guesses = state.guesses.toSeq.sorted.mkString(" ")
    val gallows = gallowsStages(state.numWrongGuesses)

    println(s"word: $word")
    println(s"guesses: $guesses")
    println(gallows)
  }

  def printInvalidInputMessage = println(
    "Invalid input, please enter a single character"
  )

  def printDuplicateGuessMessage(guess: Char) = println(
    s"You've already guessed '$guess', please enter a different character"
  )

  def printGuessedWordMessage(word: String): Unit = println(
    s"Nice one! You guessed the word '$word'."
  )

  def printRanOutOfGuessesMessage(word: String): Unit = println(
    s"Uh-oh! You ran out of guesses! The word was '$word'."
  )

  val gallowsStages = List(
    """""",
    """
  +---+
  |   |
      |
      |
      |
      |
=========""",
    """
  +---+
  |   |
  O   |
      |
      |
      |
=========""",
    """
  +---+
  |   |
  O   |
  |   |
      |
      |
=========""",
    """
  +---+
  |   |
  O   |
 /|   |
      |
      |
=========""",
    """
  +---+
  |   |
  O   |
 /|\  |
      |
      |
=========""",
    """
  +---+
  |   |
  O   |
 /|\  |
 /    |
      |
=========""",
    """
  +---+
  |   |
  O   |
 /|\  |
 / \  |
      |
========="""
  )
}
