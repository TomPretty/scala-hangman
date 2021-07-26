package example

import scala.io.StdIn.readLine

object Utils {
  def showPrompt: Unit = print("What's your guess? ")

  def readInput: Option[Letter] = {
    val input = readLine()

    for {
      char <- parseChar(input)
      letter <- parseLetter(char)
    } yield letter
  }

  def parseChar(string: String): Option[Char] = {
    if (string.length == 1) Some(string(0)) else None
  }

  def parseLetter(char: Char): Option[Letter] = {
    Letter.fromChar(char)
  }

  def printGameSummary(state: GameState): Unit = {
    val word = state.word
      .map(c => if (state.guesses.contains(c)) Letter.toChar(c) else '_')
      .mkString(" ")
    val guesses = state.guesses.toSeq.map(Letter.toChar).sorted.mkString(" ")
    val gallows = gallowsStages(state.numWrongGuesses)

    println(s"word: $word")
    println(s"guesses: $guesses")
    println(gallows)
  }

  def printInvalidInputMessage = println(
    "Invalid input, please enter a single character"
  )

  def printDuplicateGuessMessage(guess: Letter) = println(
    s"You've already guessed '${Letter.toChar(guess)}', please enter a different character"
  )

  def printGuessedWordMessage(state: GameState): Unit = println(
    s"Nice one! You guessed the word '${state.wordAsString}'."
  )

  def printRanOutOfGuessesMessage(state: GameState): Unit = println(
    s"Uh-oh! You ran out of guesses! The word was '${state.wordAsString}'."
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
