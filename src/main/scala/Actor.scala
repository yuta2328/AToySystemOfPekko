import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.persistence.typed.PersistenceId
import org.apache.pekko.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import java.time.LocalDateTime
import Utils.initOrUpdate

object OrderingActor {
  sealed trait Command
  case class GetUser(userId: String, replyTo: ActorRef[Either[String, User]]) extends Command
  case class RegisterUser(name: String, password: String, replyTo: ActorRef[Either[String, User]]) extends Command
  case class GetOrderListCommand(user: User, replyTo: ActorRef[Either[User, List[OrderRecord]]]) extends Command
  case class AddOrderCommand(orderRecord: OrderRecord) extends Command
  case class GetShoppingCartCommand(user: User, replyTo: ActorRef[Either[String, ShoppingCart]]) extends Command
  case class AddItemToShoppingCartCommand(user: User, orderedItem: OrderedItem, replyTo: ActorRef[Either[String, Unit]]) extends Command
  case class DeleteItemFromShoppingCartCommand(user: User, index: Int, replyTo: ActorRef[Either[String, Unit]]) extends Command
  case class GetRecommendedItemsCommand(user: User, replyTo: ActorRef[Either[String, RecommendedItems]]) extends Command
  case class GetStockListCommand(itemManager: ItemManager, replyTo: ActorRef[Either[String, StockList]]) extends Command
  case class AddStockCommand(stock: Stock, replyTo: ActorRef[Either[String, Unit]]) extends Command
  case class UpdateStockCommand(stock: Stock, replyTo: ActorRef[Either[String, Unit]]) extends Command

  sealed trait Event
  case class AddUser(user: User) extends Event
  case class AddOrder(orderRecord: OrderRecord) extends Event
  case class AddItemToShoppingCart(user: User, orderedItem: OrderedItem) extends Event
  case class DeleteItemFromShoppingCart(user: User, index: Int) extends Event
  case class AddStock(stock: Stock) extends Event
  case class UpdateStock(stock: Stock) extends Event

  case class State(stockList: List[Stock], orderMap: Map[User, List[OrderRecord]], shoppingCartMap: Map[User, ShoppingCart])

  private def eventHandler(state: State, event: Event): State = {
    event match {
      case AddUser(user) => state.copy(userMap = state.userMap + (user.userId -> user))

      case AddOrder(orderRecord) => state.copy(orderMap = state.orderMap + (orderRecord.user -> (state.orderMap.getOrElse(orderRecord.user, Nil) :+ orderRecord)))

      case AddItemToShoppingCart(user, orderedItem) =>
        state.copy(shoppingCartMap = Utils.initOrUpdate(state.shoppingCartMap, user, ShoppingCart(Nil, user), _.add(orderedItem)))

      case DeleteItemFromShoppingCart(user, index) => if state.shoppingCartMap.contains(user) then state.copy(shoppingCartMap = state.shoppingCartMap + (user -> state.shoppingCartMap(user).remove(index))) else state

      case AddStock(stock) => state.copy(stockList = stock :: state.stockList)

      case UpdateStock(stock) => state.copy(stockList = state.stockList.map(s => if s.stockId == stock.stockId then stock else s))
    }
  }

  private def doesUserExist[T](state: State, user: User, replyTo: ActorRef[Either[String, T]], k: Effect[Event, State]): Effect[Event, State] =
    if !state.userMap.contains(user.userId) then Effect.reply(replyTo)(Left("Not found the user")) else k

  private def commandHandler(state: State, command: Command): Effect[Event, State] = {
    command match {
      case GetOrderListCommand(user, replyTo) =>
        Effect.reply(replyTo)(if state.orderMap.contains(user) then Right(state.orderMap(user)) else Left(user))
        
      case AddOrderCommand(orderRecord) => Effect.persist(AddOrder(orderRecord))

      case GetShoppingCartCommand(user, replyTo) =>
        val shoppingCart = state.shoppingCartMap.getOrElse(user, ShoppingCart(Nil, user))
        Effect.reply(replyTo)(Right(shoppingCart))

      case AddItemToShoppingCartCommand(user, orderedItem, replyTo) =>
        Effect.persist(AddItemToShoppingCart(user, orderedItem)).thenReply(replyTo)(_ => Right(()))

      case DeleteItemFromShoppingCartCommand(user, index, replyTo) =>
        if state.shoppingCartMap.contains(user) then
          Effect.persist(DeleteItemFromShoppingCart(user, index)).thenReply(replyTo)(_ => Right(()))
        else
          Effect.reply(replyTo)(Left("User not found"))

      case GetRecommendedItemsCommand(user, replyTo) =>
        // For simplicity, we return an empty list of recommended items
        Effect.reply(replyTo)(Right(RecommendedItems(Nil, user)))

      case GetStockListCommand(itemManager, replyTo) =>
        val stockList = state.stockList.filter(_.itemManager == itemManager)
        Effect.reply(replyTo)(Right(StockList(stockList, itemManager)))

      case AddStockCommand(stock, replyTo) =>
        Effect.persist(AddStock(stock)).thenReply(replyTo)(_ => Right(()))

      case UpdateStockCommand(stock, replyTo) =>
        Effect.persist(UpdateStock(stock)).thenReply(replyTo)(_ => Right(()))
    }
  }
}
