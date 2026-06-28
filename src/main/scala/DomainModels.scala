import java.time.LocalDateTime
import java.util.UUID

case class Price(value: Int) {
  def apply(value: Int): Either[String, Price] =
    if value < 0 then Left("Incorrect value of the price") else Right(Price(value))

  def +(that: Price) = Price(value + that.value)

  def *!(that: Count) = Price(value * that.value)

  def <=(that: Price) = value <= that.value
}
val zero: Price = Price(0)

case class Count(value: Int) {
  def apply(value: Int): Either[String, Count] =
    if value < 0 then Left("Incorrect value of the price") else Right(Count(value))

  def <=(that: Count) = value <= that.value

  def >(that: Count) = value > that.value

  def +(that: Count) = Count(value + that.value)

  def -:(that: Count): Either[String, Count] =
    if value < that.value then
      Left("The left-side value is less than the right-side one")
    else Right(Count(value - that.value))
}

// End users and items
case class User(userId: String, name: String, password: String)

case class ItemManager(itemManagerId: String, name: String)

case class Item(itemId: String, name: String, price: Price)

// Models for managing stock

case class Stock(stockId: String, item: Item, count: Count, itemManager: ItemManager, registered: LocalDateTime, lastUpdated: LocalDateTime) {
  def isInStock(thatCount: Count): Boolean = thatCount <= count

  def choose(thatCount: Count): Either[String, Stock] =
    (count -: thatCount).flatMap(newCount => Right(copy(count = newCount)))

  def add(thatCount: Count): Stock = copy(count = count + thatCount)
}

case class ManagedStock(stockMap: Map[ItemManager, List[Stock]]) {
  private def getRegisteredStockList(itemManager: ItemManager): Either[String, List[Stock]] =
    stockMap.get(itemManager) match {
      case None => Left("The item manager is not registered")
      case Some(stockList) => Right(stockList)
    }

  def registerManager(itemManager: ItemManager): ManagedStock =
    if stockMap.contains(itemManager) then this
    else copy(stockMap = stockMap + (itemManager -> List()))

  def addNewStock(itemManager: ItemManager, stock: Stock): Either[String, ManagedStock] =
    getRegisteredStockList(itemManager).flatMap(stockList =>
      if stockList.exists(_.stockId == stock.stockId) then Left("The id of new stock already exists")
      else if !(stock.itemManager == itemManager) then Left("The item manager of the stock is not matched with the given one")
      else Right(copy(stockMap = stockMap + (itemManager -> (stock :: stockList))))
    )

  def choose(itemManager: ItemManager, itemId: String, count: Count): Either[String, (Stock, ManagedStock)] =
    getRegisteredStockList(itemManager).flatMap(stockList =>
      stockList.find(_.item.itemId == itemId) match {
        case None => Left("The item is not found in the stock list of the item manager")
        case Some(stock) =>
          stock.choose(count).flatMap(newStock =>
            updateStock(itemManager, newStock).map((newStock, _))
          )
      }
    )

  def updateStock(itemManager: ItemManager, stock: Stock): Either[String, ManagedStock] =
    getRegisteredStockList(itemManager).flatMap(stockList =>
      if stockList.exists(_.stockId == stock.stockId) then
        Right(copy(stockMap = stockMap + (itemManager -> stockList.map(s => if s.stockId == stock.stockId then stock else s))))
      else Left("The stock is not found in the stock list of the item manager")
    )

  def isInStock(itemManager: ItemManager, itemId: String, count: Count): Either[String, Boolean] =
    getRegisteredStockList(itemManager).map(stockList =>
      stockList.find(_.item.itemId == itemId) match {
        case None => false
        case Some(stock) => stock.isInStock(count)
      }
    )
}

// Models for recommendation
trait RecommendationStrategy {
  def recommend(user: User, managedStock: ManagedStock): Either[String, List[Stock]]
}

case class DummyRecommendationStrategy() extends RecommendationStrategy {
  def recommend(user: User, managedStock: ManagedStock): Either[String, List[Stock]] = {
    val stockList = managedStock.stockMap.values.flatten.toList
    Right(stockList.take(10))
  }
}

// Models for ordering
case class OrderInput(orderedItemList: OrderedItemList, payment: Payment, managedStock: ManagedStock)

case class OrderOutput(orderRecord: OrderRecord, managedStock: ManagedStock)

case class OrderedItem(count: Count, stock: Stock) {
  def prices(): Price = stock.item.price *! count
}
def construct(count: Count, stock: Stock): Either[String, OrderedItem] =
  if count <= stock.count then Right(OrderedItem(count, stock))
  else Left("The count of ordered item is more than the stock count")

case class OrderedItemList(orderedItemList: List[OrderedItem]) {
  def apply(countAndStockList: List[(Count, Stock)]): Either[String, OrderedItemList] =
    Utils.forM(countAndStockList, { (count, stock) => construct(count, stock) }).flatMap(l => Right(OrderedItemList(l)))
  def prices(): Price = orderedItemList.foldLeft(zero)((acc, orderedItem) => acc + orderedItem.prices())
}

case class StockDiff(stockId: String, diffCount: Count)

case class StockDiffList(stockDiffList: List[StockDiff]) {
  def apply(stockDiffList: List[StockDiff]): Either[String, StockDiffList] = {
    if stockDiffList.isEmpty then Left("The list of stock diff is empty")
    else Right(StockDiffList(stockDiffList))
  }
}

object Ordering {
  def order(orderInput: OrderInput): Either[String, OrderOutput] = {
    if !(orderInput.payment.price <= orderInput.orderedItemList.prices())
    then Left("The price of payment is not enough to pay for the ordered items")
    else if !orderInput.orderedItemList.isInStock()
    then Left("Some of the ordered items are in out stock")
    else
      Right(
        OrderOutput(
          OrderRecord(
            orderId = UUID.randomUUID().toString,
            itemList = orderInput.orderedItemList,
            payment = orderInput.payment,
            user = orderInput.payment.by,
            dateOn = LocalDateTime.now()
          ),
          stockDiffList = orderInput.orderedItemList.stockDiffList()
        )
      )
  }
}

trait PaymentMethod
case class Dummy() extends PaymentMethod

case class Payment(paymentId: String, price: Price, by: User, method: PaymentMethod)

case class OrderRecord(orderId: String, itemList: OrderedItemList, payment: Payment, user: User, dateOn: LocalDateTime)

// Models for shopping cart
case class ShoppingCart(selectedItems: List[OrderedItem], user: User) {
  def add(orderedItem: OrderedItem): ShoppingCart = ShoppingCart(selectedItems :+ orderedItem, user)
  def select(indexList: List[Int]): ShoppingCart = {
    val selected = indexList.flatMap(i => if i >= 0 && i < selectedItems.length then Some(selectedItems(i)) else None)
    ShoppingCart(selected, user)
  }
  def remove(index: Int): ShoppingCart = {
    if index >= 0 && index < selectedItems.length then
      ShoppingCart(selectedItems.patch(index, Nil, 1), user)
    else this
  }
}

def initShoppingCart(user: User): ShoppingCart = ShoppingCart(Nil, user)
