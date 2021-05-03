package akka

import akka.actor._

object BinaryTreeSet {

  sealed trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  object Operation {
    final case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation
    final case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation
    final case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation
  }

  sealed trait OperationReply {
    def id: Int
  }

  object OperationReply {
    final case class ContainsResult(id: Int, result: Boolean) extends OperationReply
    final case class OperationFinished(id: Int) extends OperationReply
  }
}

final class BinaryTreeSet extends Actor {
  import BinaryTreeSet._

  private val root = createRoot

  override def receive: Receive = {
    case m: Operation => root ! m
  }

  private def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
}
