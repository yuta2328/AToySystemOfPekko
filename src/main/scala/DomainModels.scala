import java.time.LocalDateTime
import java.util.UUID

// Primtives for models
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

  def -:(that: Count): Either[String, Count] = if value < that.value then
    Left("The left-side value is less than the right-side one")
  else Right(Count(value - that.value))
}

// End users and items
case class User(userId: String, name: String, password: String)
case class ItemManager(itemManagerId: String, name: String)

case class Item(itemId: String, name: String, price: Price, manager: ItemManager)

// Models for managing stock

case class Stock(stockId: String, item: Item, count: Count, itemManager: ItemManager, registered: LocalDateTime, lastUpdated: LocalDateTime) {
  def isInStock(thatCount: Count): Boolean = thatCount <= count

  def choose(thatCount: Count): Either[String, Stock] =
    (count -: thatCount).flatMap(newCount => Right(copy(count = newCount)))

  def add(thatCount: Count): Stock = copy(count = count + thatCount)
}

case class StockList(stockList: List[Stock], itemManager: ItemManager) {
  def apply(itemManager: ItemManager) = StockList(List(), itemManager)

  private def pick(stockId: String): Either[String, Stock] = {
    stockList.filter(_.stockId == stockId).headOption match {
      case None        => Left("Not found")
      case Some(stock) => Right(stock)
    }
  }

  private def update(stock: Stock): Either[String, StockList] =
    Utils
      .forM(stockList, stockArg => if stockArg.stockId == stock.stockId then Right(stock) else Right(stockArg))
      .flatMap(stockList1 => Right(copy(stockList = stockList1)))

  def addNewStock(stock: Stock): Either[String, StockList] = {
    if stockList.exists(_.stockId == stock.stockId) then Left("The id of new stock already exists")
    else Right(StockList(stock :: stockList, itemManager))
  }

  def choose(stockId: String, count: Count): Either[String, (Stock, StockList)] = {
    pick(stockId).flatMap(stock => stock.choose(count).flatMap(newStock => update(newStock).map((newStock, _))))
  }

  def isInStock(stockId: String, count: Count): Either[String, Boolean] =
    pick(stockId).flatMap(s => Right(s.isInStock(count)))
}

// Models for recommendation
case class RecommendedItems(itemList: List[Item], user: User)

// Models for ordering
case class OrderInput(orderedItemList: OrderedItemList, payment: Payment, stockList: StockList)

case class OrderOutput(orderRecord: OrderRecord, stockDiffList: StockDiffList)

case class OrderedItem(item: Item, count: Count, stock: Stock) {
  def apply(item: Item, count: Count, stock: Stock): Either[String, OrderedItem] = {
    if !(item.itemId == stock.item.itemId) then Left("The item and the stock are not matched")
    else Right(OrderedItem(item, count, stock))
  }
  def prices(): Price = item.price *! count
  def isInStock(): Boolean = count <= stock.count
  def stockDiff(): StockDiff = StockDiff(stock.stockId, count)
}

case class OrderedItemList(orderedItemList: List[OrderedItem]) {
  def prices(): Price = orderedItemList.foldLeft(zero)((acc, orderedItem) => acc + orderedItem.prices())
  def isInStock(): Boolean = orderedItemList.forall(_.isInStock())
  def stockDiffList(): StockDiffList = StockDiffList(orderedItemList.map(_.stockDiff()))
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
}

def initShoppingCart(user: User): ShoppingCart = ShoppingCart(Nil, user)
