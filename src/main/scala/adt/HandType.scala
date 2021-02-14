package adt

sealed trait HandType
object HandType {
  final case object Texas extends HandType
  final case object Omaha extends HandType
}
