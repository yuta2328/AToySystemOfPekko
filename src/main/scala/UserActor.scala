import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.persistence.typed.PersistenceId
import org.apache.pekko.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import java.time.LocalDateTime
import ActorUtils.*

object UserActor {
  // Commands
  sealed trait UserCommand
  case class GetUser(userId: String, replyTo: ActorRef[Either[String, User]]) extends UserCommand
  case class RegisterUser(name: String, password: String, replyTo: ActorRef[Either[String, User]]) extends UserCommand

  sealed trait UserEvent
  case class UserRegistered(user: User, timestamp: LocalDateTime) extends UserEvent

  case class State(userMap: Map[String, User])

  def apply(): Behavior[UserCommand] =
    EventSourcedBehavior[UserCommand, UserEvent, State](
      persistenceId = PersistenceId.ofUniqueId("user-actor"),
      emptyState = State(Map.empty),
      commandHandler = commandHandler,
      eventHandler = eventHandler
    )

  private def commandHandler(state: State, command: UserCommand): Effect[UserEvent, State] =
    command match {
      case GetUser(userId, replyTo) =>
        state.userMap.get(userId) match {
          case Some(user) => Effect.reply(replyTo)(Right(user))
          case None       => Effect.reply(replyTo)(Left("User not found"))
        }

      case RegisterUser(name, password, replyTo) =>
        val userId = java.util.UUID.randomUUID().toString
        val user = User(userId, name, password)
        val event = UserRegistered(user, now())
        Effect.persist(event).thenReply(replyTo)(_ => Right(user))
    }

  private def eventHandler(state: State, event: UserEvent): State =
    event match {
      case UserRegistered(user, timestamp) => State(state.userMap + (user.userId -> user))
    }
}
