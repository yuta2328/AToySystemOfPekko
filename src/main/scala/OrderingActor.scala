import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.persistence.typed.PersistenceId
import org.apache.pekko.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import java.time.LocalDateTime

object OrderingActor {
  sealed trait OrderingCommand
  case class GetOrderListCommand(user: User, replyTo: ActorRef[Either[User, List[OrderRecord]]]) extends OrderingCommand
  case class AddOrderCommand(orderRecord: OrderRecord, replyTo: ActorRef[Either[String, Unit]]) extends OrderingCommand
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

  case class State(stockListMap: Map[ItemManager, StockList], orderMap: Map[User, List[OrderRecord]], shoppingCartMap: Map[User, ShoppingCart])

  private def eventHandler(state: State, event: OrderingEvent): State = {
    event match {
      case AddOrder(orderRecord) =>
        state.copy(orderMap = state.orderMap + (orderRecord.user -> (state.orderMap.getOrElse(orderRecord.user, Nil) :+ orderRecord)))

      case AddItemToShoppingCart(user, orderedItem) =>
        val updatedCart = state.shoppingCartMap.getOrElse(user, ShoppingCart(Nil, user))
        val newCart = updatedCart.copy(selectedItems = updatedCart.selectedItems :+ orderedItem)
        state.copy(shoppingCartMap = state.shoppingCartMap + (user -> newCart))

      case DeleteItemFromShoppingCart(user, index) =>
        val updatedCart = state.shoppingCartMap.getOrElse(user, ShoppingCart(Nil, user))
        if (index >= 0 && index < updatedCart.selectedItems.length) {
          val newSelectedItems = updatedCart.selectedItems.patch(index, Nil, 1)
          val newCart = updatedCart.copy(selectedItems = newSelectedItems)
          state.copy(shoppingCartMap = state.shoppingCartMap + (user -> newCart))
        } else {
          state // No change if index is out of bounds
        }

      case AddStock(stock) =>
        val itemManager = stock.itemManager
        val updatedStockList = state.stockListMap.getOrElse(itemManager, StockList(Nil, itemManager))
        val newStockList = updatedStockList.copy(stocks =  add)
        state.copy(stockListMap = state.stockListMap + (itemManager -> newStockList))

      case UpdateStock(stock) =>
        val itemManager = stock.itemManager
        val updatedStockList = state.stockListMap.getOrElse(itemManager, StockList(itemManager, Nil))
        val newStocks = updatedStockList.stocks.map(s => if (s.itemId == stock.itemId) stock else s)
        val newStockList = updatedStockList.copy(stocks = newStocks)
        state.copy(stockListMap = state.stockListMap + (itemManager -> newStockList))
    }
}
