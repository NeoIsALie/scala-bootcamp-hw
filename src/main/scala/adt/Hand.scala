package adt

final case class Hand (cards: Set[Card])
object Hand {
  def create (cards: Set[Card], handType: HandType): Option[Hand] = handType match {
    case Texas => cards match {
      case _ if cards.size != 2 => None
      case _ => Some(Hand(cards))
    }

    case Omaha => cards match {
      case _ if cards.size != 4 => None
      case _ => Some(Hand(cards))
    }
  }
}