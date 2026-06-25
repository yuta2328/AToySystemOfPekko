import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.persistence.typed.PersistenceId
import org.apache.pekko.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import java.time.LocalDateTime

object OrderingActor {
  sealed trait OrderingCommand
  case class GetOrderListCommand(user: User, replyTo: ActorRef[Either[User, List[OrderRecord]]]) extends OrderingCommand
  case class AddOrderCommand(orderRecord, replyTo: ActorRef[Either[String, Unit]]) extends OrderingCommand
  case class GetShoppingCartCommand(user: User, replyTo: ActorRef[Either[String, ShoppingCart]]) extends OrderingCommand
  case class AddItemToShoppingCartCommand(user: User, orderedItem: OrderedItem, replyTo: ActorRef[Either[String, Unit]]) extends OrderingCommand
  case class DeleteItemFromShoppingCartCommand(user: User, index: Int, replyTo: ActorRef[Either[String, Unit]]) extends OrderingCommand
  case class GetRecommendedItemsCommand(user: User, replyTo: ActorRef[Either[String, RecommendedItems]]) extends OrderingCommand
  case class GetStockListCommand(itemManager: ItemManager, replyTo: ActorRef[Either[String, StockList]]) extends OrderingCommand
  case class AddStockCommand(stock: Stock, replyTo: ActorRef[Either[String, Unit]]) extends OrderingCommand
  case class UpdateStockCommand(stock: Stock, replyTo: ActorRef[Either[String, Unit]]) extends OrderingCommand

  sealed trait OrderingEvent
  case class AddOrder(orderRecord: OrderRecord) extends OrderingEvent
  case class AddItemToShoppingCart(user: User, orderedItem: OrderedItem) extends OrderingEvent
  case class DeleteItemFromShoppingCart(user: User, index: Int) extends OrderingEvent
  case class AddStock(stock: Stock) extends OrderingEvent
  case class UpdateStock(stock: Stock) extends OrderingEvent

  case class State(stockListMap: Map[ItemManagerUser, StockList], orderMap: Map[User, List[OrderRecord]], shoppingCartMap: Map[User, ShoppingCart])

  private def eventHandler(state: State, event: OrderingEvent): State = {
    event match {
      case AddOrder(orderRecord) =>
        state.copy(orderMap = state.orderMap + (orderRecord.user -> (state.orderMap.getOrElse(orderRecord.user, List()) :+ orderRecord)))

      case AddItemToShoppingCart(user, orderedItem) =>
        val updatedCart = state.shoppingCartMap.getOrElse(user, ShoppingCart(user, List()))
        val newCart = updatedCart.copy(items = updatedCart.items :+ orderedItem)
        state.copy(shoppingCartMap = state.shoppingCartMap + (user -> newCart))

      case DeleteItemFromShoppingCart(user, index) =>
        val updatedCart = state.shoppingCartMap.getOrElse(user, ShoppingCart(user, List()))
        if (index >= 0 && index < updatedCart.items.length) {
          val newItems = updatedCart.items.patch(index, Nil, 1)
          val newCart = updatedCart.copy(items = newItems)
          state.copy(shoppingCartMap = state.shoppingCartMap + (user -> newCart))
        } else {
          state // No change if index is out of bounds
        }

      case AddStock(stock) =>
        val itemManagerUser = stock.itemManagerUser
        val updatedStockList = state.stockListMap.getOrElse(itemManagerUser, StockList(itemManagerUser, List()))
        val newStockList = updatedStockList.copy(stocks = updatedStockList.stocks :+ stock)
        state.copy(stockListMap = state.stockListMap + (itemManagerUser -> newStockList))

      case UpdateStock(stock) =>
        val itemManagerUser = stock.itemManagerUser
        val updatedStockList = state.stockListMap.getOrElse(itemManagerUser, StockList(itemManagerUser, List()))
        val newStocks = updatedStockList.stocks.map(s => if (s.itemId == stock.itemId) stock else s)
        val newStockList = updatedStockList.copy(stocks = newStocks)
        state.copy(stockListMap = state.stockListMap + (itemManagerUser -> newStockList))
    }
}
