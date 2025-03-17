package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import java.awt.color.ColorSpace
import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h, t) if n == 0   => Cons(h, skip(t)(n))
      case Cons(_, t)             => skip(t)(n - 1)
      case _                      => Nil()

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
      case Cons(hFirst, tFirst) => second match
        case Cons(hSec, tSec) if tSec != Nil() => Cons((hFirst, hSec), zip(tFirst, tSec))
        case Cons(hSec, _) => Cons((hFirst, hSec), Nil())
        case _ => Nil()
      case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h, t) => Cons(h, concat(skip(s1)(1), s2))
      case _ => s2 match
        case Cons(h, t) => Cons(h, concat(Nil(), skip(s2)(1)))
        case _ => Nil()

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _rev(init: Sequence[A], out: Sequence[A]): Sequence[A] = init match
        case Nil() => out
        case Cons(h, t) => _rev(t, Cons(h, out))
      _rev(s, Nil())

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      @tailrec
      def _flat(init: Sequence[A], out: Sequence[B]): Sequence[B] = init match
        case Nil() => out
        case Cons(h, t) => _flat(t, concat(out, mapper(h)))
      _flat(s, Nil())

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      @tailrec
      def _min(seq: Sequence[Int], min: Optional[Int]): Optional[Int] = seq match
        case Nil() if orElse(min, 0) == 0 => Empty()
        case Nil() => min
        case Cons(h, t) if h < orElse(min, 1) => _min(t, Just(h))
        case Cons(_, t) => _min(t, min)
      _min(s, Just(sum(s)))

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      def _even(seq: Sequence[A], i: Int): Sequence[A] = seq match
        case Nil() => seq
        case Cons(h, t) if i % 2 == 0 => Cons(h, _even(t, i + 1))
        case Cons(_, t) => _even(t, i + 1)
      _even(s, 0)

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) if h == elem => true
      case Cons(_, t) => contains(t)(elem)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _dist(seq: Sequence[A], out: Sequence[A]): Sequence[A] = seq match
        case Nil() => out
        case Cons(h, t) if !contains(out)(h) => _dist(t, concat(out, Cons(h, Nil())))
        case Cons(_, t) => _dist(t, out)
      _dist(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = ???

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def _part(seq: Sequence[A], out1: Sequence[A], out2: Sequence[A]): (Sequence[A], Sequence[A]) = seq match
        case Cons(h, t) if pred(h) => _part(t, concat(out1, Cons(h, Nil())), out2)
        case Cons(h, t) => _part(t, out1, concat(out2, Cons(h, Nil())))
        case _ => (out1, out2)
      _part(s, Nil(), Nil())

  end Sequence
end Sequences

@main def trySequences =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52
