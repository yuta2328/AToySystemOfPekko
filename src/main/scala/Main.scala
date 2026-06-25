import org.apache.pekko.actor.typed.{ActorRef, ActorSystem}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.model.{StatusCodes, headers}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive1, Route}
import org.apache.pekko.util.Timeout
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success}

// Request/Response case classes
case class LoginRequest(name: String, password: String)
case class LoginResponse(user: User, token: String)
case class LogoutRequest(userId: String)
case class LogoutResponse(message: String)
case class ListResponse(recommendedItems: RecommendedItems)
case class BuyRequest(itemAndCount: ItemAndCount, payment: Payment)
case class BuyResponse(order: Order)
case class HistoryResponse(orderList: List[Order])
case class CartListResponse(cart: ShoppingCart)
case class CartAddRequest(selectedItem: SelectedItem)
case class CartAddResponse(message: String)
case class CartDeleteRequest(index: Int)
case class CartDeleteResponse(message: String)
case class CartBuyRequest(indexList: List[Int], payment: Payment)
case class CartBuyResponse(order: Order)
case class ManagerLoginRequest(name: String, password: String)
case class ManagerLoginResponse(manager: ItemManager, token: String)
case class ManagerLogoutRequest(managerId: String)
case class ManagerLogoutResponse(message: String)
case class ManagerListResponse(stockList: StockList)
case class ManagerAddRequest(item: Item, initialStock: Count)
case class ManagerAddResponse(item: Item, stock: Stock)
case class ManagerDeleteRequest(itemId: String)
case class ManagerDeleteResponse(message: String)
case class ErrorResponse(error: String)

// JSON Protocol for spray-json
trait JsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val priceFormat: RootJsonFormat[Price] = jsonFormat1(Price.apply)
  implicit val countFormat: RootJsonFormat[Count] = jsonFormat1(Count.apply)
  implicit val userFormat: RootJsonFormat[User] = jsonFormat2(User.apply)
  implicit val itemManagerFormat: RootJsonFormat[ItemManager] = jsonFormat2(ItemManager.apply)
  implicit val itemFormat: RootJsonFormat[Item] = jsonFormat4(Item.apply)
  implicit val localDateTimeFormat: RootJsonFormat[java.time.LocalDateTime] =
    new RootJsonFormat[java.time.LocalDateTime] {
      import spray.json.*
      def write(dt: java.time.LocalDateTime): JsValue = JsString(dt.toString)
      def read(value: JsValue): java.time.LocalDateTime = value match {
        case JsString(s) => java.time.LocalDateTime.parse(s)
        case _           => throw new RuntimeException("Invalid LocalDateTime format")
      }
    }
  implicit val stockFormat: RootJsonFormat[Stock] = jsonFormat4(Stock.apply)
  implicit val orderingFormat: RootJsonFormat[Ordering] = jsonFormat3(Ordering.apply)
  implicit val paymentMethodFormat: RootJsonFormat[PaymentMethod] = new RootJsonFormat[PaymentMethod] {
    import spray.json.*
    def write(pm: PaymentMethod): JsValue = pm match {
      case Dummy() => JsObject("type" -> JsString("Dummy"))
    }
    def read(value: JsValue): PaymentMethod = value match {
      case JsObject(fields) if fields.get("type").contains(JsString("Dummy")) => Dummy()
      case _                                                                  => Dummy()
    }
  }
  implicit val paymentFormat: RootJsonFormat[Payment] = jsonFormat4(Payment.apply)
  implicit val orderFormat: RootJsonFormat[Order] = jsonFormat5(Order.apply)
  implicit val shoppingCartFormat: RootJsonFormat[ShoppingCart] = jsonFormat2(ShoppingCart.apply)
  implicit val recommendedItemsFormat: RootJsonFormat[RecommendedItems] = jsonFormat2(RecommendedItems.apply)
  implicit val stockListFormat: RootJsonFormat[StockList] = jsonFormat2(StockList.apply)
  implicit val itemAndCountFormat: RootJsonFormat[ItemAndCount] = jsonFormat2(ItemAndCount.apply)
  implicit val selectedItemFormat: RootJsonFormat[SelectedItem] = jsonFormat2(SelectedItem.apply)

  // Request/Response formats
  implicit val loginRequestFormat: RootJsonFormat[LoginRequest] = jsonFormat2(LoginRequest.apply)
  implicit val loginResponseFormat: RootJsonFormat[LoginResponse] = jsonFormat2(LoginResponse.apply)
  implicit val logoutRequestFormat: RootJsonFormat[LogoutRequest] = jsonFormat1(LogoutRequest.apply)
  implicit val logoutResponseFormat: RootJsonFormat[LogoutResponse] = jsonFormat1(LogoutResponse.apply)
  implicit val listResponseFormat: RootJsonFormat[ListResponse] = jsonFormat1(ListResponse.apply)
  implicit val buyRequestFormat: RootJsonFormat[BuyRequest] = jsonFormat2(BuyRequest.apply)
  implicit val buyResponseFormat: RootJsonFormat[BuyResponse] = jsonFormat1(BuyResponse.apply)
  implicit val historyResponseFormat: RootJsonFormat[HistoryResponse] = jsonFormat1(HistoryResponse.apply)
  implicit val cartListResponseFormat: RootJsonFormat[CartListResponse] = jsonFormat1(CartListResponse.apply)
  implicit val cartAddRequestFormat: RootJsonFormat[CartAddRequest] = jsonFormat1(CartAddRequest.apply)
  implicit val cartAddResponseFormat: RootJsonFormat[CartAddResponse] = jsonFormat1(CartAddResponse.apply)
  implicit val cartDeleteRequestFormat: RootJsonFormat[CartDeleteRequest] = jsonFormat1(CartDeleteRequest.apply)
  implicit val cartDeleteResponseFormat: RootJsonFormat[CartDeleteResponse] = jsonFormat1(CartDeleteResponse.apply)
  implicit val cartBuyRequestFormat: RootJsonFormat[CartBuyRequest] = jsonFormat2(CartBuyRequest.apply)
  implicit val cartBuyResponseFormat: RootJsonFormat[CartBuyResponse] = jsonFormat1(CartBuyResponse.apply)
  implicit val managerLoginRequestFormat: RootJsonFormat[ManagerLoginRequest] = jsonFormat2(ManagerLoginRequest.apply)
  implicit val managerLoginResponseFormat: RootJsonFormat[ManagerLoginResponse] = jsonFormat2(
    ManagerLoginResponse.apply
  )
  implicit val managerLogoutRequestFormat: RootJsonFormat[ManagerLogoutRequest] = jsonFormat1(
    ManagerLogoutRequest.apply
  )
  implicit val managerLogoutResponseFormat: RootJsonFormat[ManagerLogoutResponse] = jsonFormat1(
    ManagerLogoutResponse.apply
  )
  implicit val managerListResponseFormat: RootJsonFormat[ManagerListResponse] = jsonFormat1(ManagerListResponse.apply)
  implicit val managerAddRequestFormat: RootJsonFormat[ManagerAddRequest] = jsonFormat2(ManagerAddRequest.apply)
  implicit val managerAddResponseFormat: RootJsonFormat[ManagerAddResponse] = jsonFormat2(ManagerAddResponse.apply)
  implicit val managerDeleteRequestFormat: RootJsonFormat[ManagerDeleteRequest] = jsonFormat1(
    ManagerDeleteRequest.apply
  )
  implicit val managerDeleteResponseFormat: RootJsonFormat[ManagerDeleteResponse] = jsonFormat1(
    ManagerDeleteResponse.apply
  )
  implicit val errorResponseFormat: RootJsonFormat[ErrorResponse] = jsonFormat1(ErrorResponse.apply)

}

object Main extends App with JsonProtocol {
  given system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "pekko-EC")
  given ec: ExecutionContext = system.executionContext
  given timeout: Timeout = 5.seconds

  // Authentication directive
  def authenticated: Directive1[String] = {
    optionalHeaderValueByName("Authorization").flatMap {
      case Some(token) =>
        val bearerToken = token.stripPrefix("Bearer ")
        JwtService.validateToken(bearerToken) match {
          case Some(userId) => provide(userId)
          case None         => complete(StatusCodes.Unauthorized, ErrorResponse("Invalid or expired token"))
        }
      case None =>
        complete(StatusCodes.Unauthorized, ErrorResponse("Missing Authorization header"))
    }
  }

  // Helper to create user actor for specific userId
  def getUserActor(userId: String): ActorRef[UserActor.UserCommand] = {
    system.systemActorOf(UserActor(userId), s"user-$userId")
  }

  // Helper to create cart actor for specific userId
  def getCartActor(userId: String, user: User): ActorRef[CartActor.CartCommand] = {
    system.systemActorOf(CartActor(userId, user), s"cart-$userId")
  }

  // Helper to create manager actor
  def getManagerActor(managerId: String): ActorRef[ManagerActor.ManagerCommand] = {
    system.systemActorOf(ManagerActor(managerId), s"manager-$managerId")
  }

  val routes: Route =
    pathPrefix("api" / "v1") {
      concat(
        // User login
        path("login") {
          post {
            entity(as[LoginRequest]) { request =>
              val userActor = getUserActor(request.name)
              val responseFuture: Future[Either[String, (User, String)]] =
                userActor.ask(UserActor.LoginCommand(request.name, request.password, _))

              onSuccess(responseFuture) {
                case Right((user, token)) =>
                  complete(LoginResponse(user, token))
                case Left(reason) =>
                  complete(StatusCodes.Unauthorized, ErrorResponse(reason))
              }
            }
          }
        },
        // User logout
        path("logout") {
          post {
            authenticated { userId =>
              entity(as[LogoutRequest]) { request =>
                val userActor = getUserActor(userId)
                val responseFuture: Future[Either[String, Unit]] =
                  userActor.ask(UserActor.LogoutCommand(userId, _))

                onSuccess(responseFuture) {
                  case Right(_) =>
                    complete(LogoutResponse("Logged out successfully"))
                  case Left(reason) =>
                    complete(StatusCodes.BadRequest, ErrorResponse(reason))
                }
              }
            }
          }
        },
        // Get recommended items
        path("list") {
          get {
            authenticated { userId =>
              val userActor = getUserActor(userId)
              val responseFuture: Future[Either[String, RecommendedItems]] =
                userActor.ask(UserActor.GetRecommendedItemsCommand(userId, _))

              onSuccess(responseFuture) {
                case Right(items) =>
                  complete(ListResponse(items))
                case Left(reason) =>
                  complete(StatusCodes.NotFound, ErrorResponse(reason))
              }
            }
          }
        },
        // Direct purchase
        path("buy") {
          post {
            authenticated { userId =>
              entity(as[BuyRequest]) { request =>
                val userActor = getUserActor(userId)
                val responseFuture: Future[Either[OrderError, Order]] =
                  userActor.ask(
                    UserActor.BuyCommand(
                      userId,
                      request.itemAndCount.item,
                      request.itemAndCount.count,
                      request.payment,
                      _
                    )
                  )

                onSuccess(responseFuture) {
                  case Right(order) =>
                    complete(BuyResponse(order))
                  case Left(error) =>
                    complete(StatusCodes.BadRequest, ErrorResponse(s"Purchase failed: $error"))
                }
              }
            }
          }
        },
        // Get order history
        path("history") {
          get {
            authenticated { userId =>
              val userActor = getUserActor(userId)
              val responseFuture: Future[Either[String, List[Order]]] =
                userActor.ask(UserActor.GetOrderHistoryCommand(userId, _))

              onSuccess(responseFuture) {
                case Right(orders) =>
                  complete(HistoryResponse(orders))
                case Left(reason) =>
                  complete(StatusCodes.NotFound, ErrorResponse(reason))
              }
            }
          }
        },
        // Shopping cart routes
        pathPrefix("cart") {
          concat(
            // List cart items
            path("list") {
              get {
                authenticated { userId =>
                  val user = User(userId, "temp") // TODO: Get actual user from UserActor
                  val cartActor = getCartActor(userId, user)
                  val responseFuture: Future[Either[String, ShoppingCart]] =
                    cartActor.ask(CartActor.GetCartCommand(userId, _))

                  onSuccess(responseFuture) {
                    case Right(cart) =>
                      complete(CartListResponse(cart))
                    case Left(reason) =>
                      complete(StatusCodes.NotFound, ErrorResponse(reason))
                  }
                }
              }
            },
            // Add item to cart
            path("add") {
              post {
                authenticated { userId =>
                  entity(as[CartAddRequest]) { request =>
                    val user = User(userId, "temp")
                    val cartActor = getCartActor(userId, user)
                    val responseFuture: Future[Either[String, Unit]] =
                      cartActor.ask(
                        CartActor.AddToCartCommand(userId, request.selectedItem.item, request.selectedItem.count, _)
                      )

                    onSuccess(responseFuture) {
                      case Right(_) =>
                        complete(CartAddResponse("Item added to cart"))
                      case Left(reason) =>
                        complete(StatusCodes.BadRequest, ErrorResponse(reason))
                    }
                  }
                }
              }
            },
            // Remove item from cart
            path("delete") {
              delete {
                authenticated { userId =>
                  entity(as[CartDeleteRequest]) { request =>
                    val user = User(userId, "temp")
                    val cartActor = getCartActor(userId, user)
                    val responseFuture: Future[Either[String, Unit]] =
                      cartActor.ask(CartActor.RemoveFromCartCommand(userId, request.index, _))

                    onSuccess(responseFuture) {
                      case Right(_) =>
                        complete(CartDeleteResponse("Item removed from cart"))
                      case Left(reason) =>
                        complete(StatusCodes.BadRequest, ErrorResponse(reason))
                    }
                  }
                }
              }
            },
            // Purchase from cart
            path("buy") {
              post {
                authenticated { userId =>
                  entity(as[CartBuyRequest]) { request =>
                    val user = User(userId, "temp")
                    val cartActor = getCartActor(userId, user)
                    val responseFuture: Future[Either[OrderError, Order]] =
                      cartActor.ask(CartActor.BuyFromCartCommand(userId, request.indexList, request.payment, _))

                    onSuccess(responseFuture) {
                      case Right(order) =>
                        // Record order in UserActor
                        val userActor = getUserActor(userId)
                        userActor ! UserActor.RecordOrderCommand(userId, order, system.ignoreRef)
                        complete(CartBuyResponse(order))
                      case Left(error) =>
                        complete(StatusCodes.BadRequest, ErrorResponse(s"Purchase failed: $error"))
                    }
                  }
                }
              }
            }
          )
        },
        // Manager routes
        pathPrefix("manager") {
          concat(
            // Manager login
            path("login") {
              post {
                entity(as[ManagerLoginRequest]) { request =>
                  val managerActor = getManagerActor(request.name)
                  val responseFuture: Future[Either[String, (ItemManager, String)]] =
                    managerActor.ask(ManagerActor.ManagerLoginCommand(request.name, request.password, _))

                  onSuccess(responseFuture) {
                    case Right((manager, token)) =>
                      complete(ManagerLoginResponse(manager, token))
                    case Left(reason) =>
                      complete(StatusCodes.Unauthorized, ErrorResponse(reason))
                  }
                }
              }
            },
            // Manager logout
            path("logout") {
              post {
                authenticated { managerId =>
                  entity(as[ManagerLogoutRequest]) { request =>
                    val managerActor = getManagerActor(managerId)
                    val responseFuture: Future[Either[String, Unit]] =
                      managerActor.ask(ManagerActor.ManagerLogoutCommand(managerId, _))

                    onSuccess(responseFuture) {
                      case Right(_) =>
                        complete(ManagerLogoutResponse("Logged out successfully"))
                      case Left(reason) =>
                        complete(StatusCodes.BadRequest, ErrorResponse(reason))
                    }
                  }
                }
              }
            },
            // List managed items
            path("list") {
              get {
                authenticated { managerId =>
                  val managerActor = getManagerActor(managerId)
                  val responseFuture: Future[Either[String, StockList]] =
                    managerActor.ask(ManagerActor.GetManagedItemsCommand(managerId, _))

                  onSuccess(responseFuture) {
                    case Right(stockList) =>
                      complete(ManagerListResponse(stockList))
                    case Left(reason) =>
                      complete(StatusCodes.NotFound, ErrorResponse(reason))
                  }
                }
              }
            },
            // Add item to catalog
            path("add") {
              post {
                authenticated { managerId =>
                  entity(as[ManagerAddRequest]) { request =>
                    val managerActor = getManagerActor(managerId)
                    val responseFuture: Future[Either[String, (Item, Stock)]] =
                      managerActor.ask(ManagerActor.AddItemCommand(managerId, request.item, request.initialStock, _))

                    onSuccess(responseFuture) {
                      case Right((item, stock)) =>
                        complete(ManagerAddResponse(item, stock))
                      case Left(reason) =>
                        complete(StatusCodes.BadRequest, ErrorResponse(reason))
                    }
                  }
                }
              }
            },
            // Remove item from catalog
            path("delete") {
              delete {
                authenticated { managerId =>
                  entity(as[ManagerDeleteRequest]) { request =>
                    val managerActor = getManagerActor(managerId)
                    val responseFuture: Future[Either[String, Unit]] =
                      managerActor.ask(ManagerActor.RemoveItemCommand(managerId, request.itemId, _))

                    onSuccess(responseFuture) {
                      case Right(_) =>
                        complete(ManagerDeleteResponse("Item removed from catalog"))
                      case Left(reason) =>
                        complete(StatusCodes.BadRequest, ErrorResponse(reason))
                    }
                  }
                }
              }
            }
          )
        }
      )
    }

  val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(routes)

  bindingFuture.onComplete {
    case Success(binding) =>
      val address = binding.localAddress
      system.log.info(s"Server online at http://${address.getHostString}:${address.getPort}/")
    case Failure(ex) =>
      system.log.error("Failed to bind HTTP endpoint, terminating system", ex)
      system.terminate()
  }
}
