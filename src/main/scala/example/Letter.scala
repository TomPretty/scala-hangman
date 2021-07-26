package example

sealed trait Letter

object A extends Letter
object B extends Letter
object C extends Letter
object D extends Letter
object E extends Letter
object F extends Letter
object G extends Letter
object H extends Letter
object I extends Letter
object J extends Letter
object K extends Letter
object L extends Letter
object M extends Letter
object N extends Letter
object O extends Letter
object P extends Letter
object Q extends Letter
object R extends Letter
object S extends Letter
object T extends Letter
object U extends Letter
object V extends Letter
object W extends Letter
object X extends Letter
object Y extends Letter
object Z extends Letter

object Letter {
  def toChar(letter: Letter): Char = letter match {
    case A => 'A'
    case B => 'B'
    case C => 'C'
    case D => 'D'
    case E => 'E'
    case F => 'F'
    case G => 'G'
    case H => 'H'
    case I => 'I'
    case J => 'J'
    case K => 'K'
    case L => 'L'
    case M => 'M'
    case N => 'N'
    case O => 'O'
    case P => 'P'
    case Q => 'Q'
    case R => 'R'
    case S => 'S'
    case T => 'T'
    case U => 'U'
    case V => 'V'
    case W => 'W'
    case X => 'X'
    case Y => 'Y'
    case Z => 'Z'
  }

  def fromChar(char: Char) = char.toUpper match {
    case 'A' => Some(A)
    case 'B' => Some(B)
    case 'C' => Some(C)
    case 'D' => Some(D)
    case 'E' => Some(E)
    case 'F' => Some(F)
    case 'G' => Some(G)
    case 'H' => Some(H)
    case 'I' => Some(I)
    case 'J' => Some(J)
    case 'K' => Some(K)
    case 'L' => Some(L)
    case 'M' => Some(M)
    case 'N' => Some(N)
    case 'O' => Some(O)
    case 'P' => Some(P)
    case 'Q' => Some(Q)
    case 'R' => Some(R)
    case 'S' => Some(S)
    case 'T' => Some(T)
    case 'U' => Some(U)
    case 'V' => Some(V)
    case 'W' => Some(W)
    case 'X' => Some(X)
    case 'Y' => Some(Y)
    case 'Z' => Some(Z)
    case _   => None
  }
}
