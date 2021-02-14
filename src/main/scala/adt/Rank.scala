package adt

sealed trait Rank {def name: String; def weight: Int}
object Rank {
  final case object A extends Rank {val name = "A"; val weight = 14}
  final case object K extends Rank {val name = "K"; val weight = 13}
  final case object Q extends Rank {val name = "Q"; val weight = 12}
  final case object J extends Rank {val name = "J"; val weight = 11}
  final case object T extends Rank {val name = "T"; val weight = 10}
  final case object Nine extends Rank {val name = "9"; val weight = 9}
  final case object Eight extends Rank {val name = "8"; val weight = 8}
  final case object Seven extends Rank {val name = "7"; val weight = 7}
  final case object Six extends Rank {val name = "6"; val weight = 6}
  final case object Five extends Rank {val name = "5"; val weight = 5}
  final case object Four extends Rank {val name = "4"; val weight = 4}
  final case object Three extends Rank {val name = "3"; val weight = 3}
  final case object Two extends Rank {val name = "2"; val weight = 2}

  def create (name : String): Option[Rank] = name match {
    case "A" => Some(A)
    case "K" => Some(K)
    case "Q" => Some(Q)
    case "J" => Some(J)
    case "T" => Some(T)
    case "9" => Some(Nine)
    case "8" => Some(Eight)
    case "7" => Some(Seven)
    case "6" => Some(Six)
    case "5" => Some(Five)
    case "4" => Some(Four)
    case "3" => Some(Three)
    case "2" => Some(Two)
    case _ => None
  }
}

