package adt

sealed trait PokerCombination
object PokerCombination {
  final case class HighCard (board: Board, hand: Hand) extends PokerCombination
  final case class Pair (board: Board, hand: Hand) extends PokerCombination
  final case class TwoPairs (board: Board, hand: Hand) extends PokerCombination
  final case class ThreeOfKind (board: Board, hand: Hand) extends PokerCombination
  final case class FullHouse (board: Board, hand: Hand) extends PokerCombination
  final case class Straight (board: Board, hand: Hand) extends PokerCombination
  final case class Flush (board: Board, hand: Hand) extends PokerCombination
  final case class FourOfKind (board: Board, hand: Hand) extends PokerCombination
  final case class StraightFlush (board: Board, hand: Hand) extends PokerCombination
}