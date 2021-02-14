package adt

final case class Board (cards: Set[Card])
object Board {
  def create (cards: Set[Card]): Option[Set[Card]] = cards match {
    case _ if cards.size != 5 => None
    case _ => Some(cards)
  }
}