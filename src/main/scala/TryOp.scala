import scala.util.control.NonFatal

object TryOp extends App {

  object Try {
    def apply[T](expr: => T): Try[T] =
      try Success(expr)
      catch {
        case ex: Exception => Failure(ex)
      }
  }

  abstract class Try[+T] {
    def flatMap[U](f: T => Try[U]): Try[U] = this match {
      case Success(x)  => try f(x) catch { case ex: Exception => Failure(ex) }
      case fail: Failure => fail
    }

    def map[U](f: T => U): Try[U] = this match {
      case Success(x) => Try(f(x))
      case fail: Failure => fail
    }

    }
    case class Success[T](x: T)       extends Try[T]
    case class Failure(ex: Exception) extends Try[Nothing]


  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
    def union(other: IntSet) = other
  }

  case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if(x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if(x < elem) NonEmpty(elem, left incl x, right)
      else if (x > elem) NonEmpty(elem, left, right incl x)
      else this

    def union(other: IntSet): IntSet = (left union (right union (other))) incl elem
  }


  /*

  To Prove:
  (xs union ys) contains x = xs contains x || ys contains x

  Given:
  1) Definitions above
  2) Empty contains x = false
  3) (s incl x) contains x = true
  4) (s incl y) contains x = s contains y ; if x != y

  Induction Step and Structural Proof using xs

  Case I:
  if xs = Empty

  Left Hand Side:
  (Empty union ys) contains x
  = ys contains x                => Definition of Empty union other

  Right Hand Side:
  Empty contains x || ys contains x
  = false || ys contains x    => Definition of Empty contains x
  = ys contains x             => Truth table of OR ( false OR true = true, false OR false = false)

  Left Hand Side = Right Hand Side hence Induction step proved, the statement is true for all subtrees

  Case II:
  if (xs is NonEmpty(z, l, r) and z = x)

  Left Hand Side:
  (NonEmpty(x, l, r) union ys) contains x
  = ((l union(r union ys)) incl x) contains x       => From definition of union on NonEmpty
  = true                                            => from (3) above  (s incl x) contains x = true

  Right Hand Side
  xs contains x || ys contains x
  = NonEmpty(x, l, r) contains x || ys contains x   => From definition of xs
  = true || ys contains x                           => From definition of contains on NonEmpty
  = true                                            => Truth table of OR (true OR false = true, true OR true = true)

  Left Hand Side = Right Hand Side

  Case III:
  if ( xs is NonEmpty(z, l, r) and z < x )

  Left Hand Side:
  (NonEmpty(z, l, r) union ys) contains x
  = (l union(r union ys) incl z) contains x           => From definition of union
  = (l union(r union ys)) contains x                  => From (4) above (s incl x) contains y = s contains y; if x != y
  = (l contains x || (r union ys) contains x)         => From induction step, the statement is true for all subtrees
  = (l contains x || r contains x || ys contains x)   => From induction step, the statement is true for all subtrees
  = r contains x || ys contains x                     => since z < x, definition of contains (l contains x) returns false


  Right Hand Side:
  (if z < x)
  NonEmpty(z, l, r) contains x || ys contains x
  = r contains x || ys contains x                     => definition of contains,

  Left Hand Side = Right Hand Side,

  The case if ( xs is NonEmpty(z, l, r) and z > x ) is analogous to Case III above

  Hence proved

   */


}
