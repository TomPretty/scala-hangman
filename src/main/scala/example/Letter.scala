package example

sealed trait Letter

object A extends Letter
object B extends Letter
object C extends Letter

object Letter {
  def toChar(letter: Letter) = letter match {
    case A => "A"
    case B => "B"
    case C => "C"
  }

  def fromChar(char: Char) = char.toUpper match {
    case 'A' => Some(A)
    case 'B' => Some(B)
    case 'C' => Some(C)
    case _   => None
  }
}
