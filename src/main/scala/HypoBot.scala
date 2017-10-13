package cc.hypo

import io.scalac.slack.MessageEventBus
import io.scalac.slack.bots.AbstractBot
import io.scalac.slack.common.{BaseMessage, Command, OutboundMessage}

import MyPostgresDriver.simple._
import com.typesafe.config.ConfigFactory
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.libs.json._
import java.sql.Timestamp

case class Book(id: Option[Int],
                title: Option[String],
                data: Option[JsValue],
                bookType: Option[String],
                user: Option[String],
                photoSource: Option[String],
                createdAt: Option[Timestamp],
                updatedAt: Option[Timestamp],
                sourceUser: Option[String],
                backend: Option[String] = Some("tw")
                 )
{
  def readable: String = s"📕${id.get}[${bookType.get}] U: ${updatedAt.get}, C: ${createdAt.get}"
}

class Books(tag: Tag) extends Table[Book](tag, "books") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def title = column[Option[String]]("title")
  def data = column[Option[JsValue]]("data")
  def bookType = column[Option[String]]("book_type")
  def user = column[Option[String]]("user")
  def photoSource = column[Option[String]]("photo_source")
  def createdAt = column[Option[Timestamp]]("created_at")
  def updatedAt = column[Option[Timestamp]]("updated_at")
  def sourceUser = column[Option[String]]("source_user")
  def backend = column[Option[String]]("backend")

  def * = (id.?,
    title,
    data,
    bookType,
    user,
    photoSource,
    createdAt,
    updatedAt,
    sourceUser,
    backend) <> (Book.tupled, Book.unapply)
}

case class HypoOrder( id: Option[Int],
                      data: Option[JsValue],
                      bookType: Option[String],
                      user: Option[String],
                      photoSource: Option[String],
                      sourceUser: Option[String],
                      saleID: Option[String],
                      createdAt: Option[Timestamp],
                      updatedAt: Option[Timestamp],
                      address: Option[String],
                      confirmed: Option[Boolean],
                      status: Option[String] = Some("pending"),
                      quantity: Option[Int] = Some(1),
                      token: Option[String],
                      backend: Option[String] = Some("tw")
                      ) {
  def toBook: (Book) =
    Book( id = None,
      title = None,
      data = data,
      bookType = bookType,
      user = user,
      photoSource = photoSource,
      createdAt = createdAt,
      updatedAt = updatedAt,
      sourceUser = sourceUser,
      backend = backend)
}

class HypoOrders(tag: Tag) extends Table[HypoOrder](tag, "hypo_orders") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def data = column[Option[JsValue]]("data")
  def bookType = column[Option[String]]("book_type")
  def user = column[Option[String]]("user")
  def photoSource = column[Option[String]]("photo_source")
  def sourceUser = column[Option[String]]("source_user")
  def saleID = column[Option[String]]("sale_id")
  def createdAt = column[Option[Timestamp]]("created_at")
  def updatedAt = column[Option[Timestamp]]("updated_at")
  def address = column[Option[String]]("address")
  def confirmed = column[Option[Boolean]]("confirmed")
  def status = column[Option[String]]("status")
  def quantity = column[Option[Int]]("quantity")
  def token = column[Option[String]]("token")
  def backend = column[Option[String]]("backend")

  def * = (id.?,
    data,
    bookType,
    user,
    photoSource,
    sourceUser,
    saleID,
    createdAt,
    updatedAt,
    address,
    confirmed,
    status,
    quantity,
    token,
    backend) <> (HypoOrder.tupled, HypoOrder.unapply)
}

case class HypoProduct( id: Option[Int],
                        productType: String,
                        saleID: Option[String],
                        backend: String = "tw",
                        status: String = "new",
                        data: Option[JsValue],
                        user: String,
                        createdAt: Option[Timestamp],
                        updatedAt: Option[Timestamp]
                      ) {
  def restore: HypoProduct = 
    copy(id = None, saleID = None, status = "new", createdAt = None, updatedAt = None)
  def restoreForUser(user: String): HypoProduct = 
    copy(id = None, saleID = None, status = "new", user = user, createdAt = None, updatedAt = None)
}

class HypoProducts(tag: Tag) extends Table[HypoProduct](tag, "hypo_products") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def productType = column[String]("product_type")
  def saleID = column[Option[String]]("sale_id")
  def backend = column[String]("backend")
  def status = column[String]("status")
  def data = column[Option[JsValue]]("data")
  def user = column[String]("user")
  def createdAt = column[Option[Timestamp]]("created_at")
  def updatedAt = column[Option[Timestamp]]("updated_at")
  
  def * = (id.?,
           productType,
           saleID,
           backend,
           status,
           data,
           user,
           createdAt,
           updatedAt) <> (HypoProduct.tupled, HypoProduct.unapply)
}

case class DateDatabase(url: String, user: String, password: String) {
  val db = Database.forURL(url = url, user = user, password = password, driver = "org.postgresql.Driver")
  val hypoProducts = TableQuery[HypoProducts]

  def productForSaleID(saleID: String) = db withSession { implicit  session: Session =>
    hypoProducts.filter(_.saleID === saleID).firstOption
  }

  def insertProduct(product: HypoProduct): Int = db withSession { implicit session: Session =>
    (hypoProducts returning hypoProducts.map(_.id)) += product
  }
}


case class EditorDatabase(url: String, user: String, password: String) {
  val db = Database.forURL(url = url, user = user, password = password, driver = "org.postgresql.Driver")

  val books = TableQuery[Books]
  val hypoOrders = TableQuery[HypoOrders]

  def orderForSaleID(saleID: String) = db withSession { implicit  session: Session =>
    hypoOrders.filter(_.saleID === saleID).firstOption
  }

  def bookForID(bookID: Int): Option[Book] = db withSession { implicit  session: Session =>
    books.filter(_.id === bookID).firstOption
  }

  def booksForUser(user: String) = db withSession { implicit session: Session =>
    (for { b <- books if b.user === user} yield b ).list
  }

  def insertBook(book: Book): Int = db withSession { implicit session: Session =>
    (books returning books.map(_.id)) += book
  }

  def deleteBookForID(bookID: Int) = db withSession { implicit session: Session =>
    books.filter(_.id === bookID).delete
  }
}

class HypoBot(override val bus: MessageEventBus) extends AbstractBot {
  val config = ConfigFactory.load()
  val editorDB = EditorDatabase(
    url = config.getString("editor.db-url"),
    user = config.getString("editor.db-user"),
    password = config.getString("editor.db-password")
  )
  val dateDB = DateDatabase(
    url = config.getString("date.db-url"),
    user = config.getString("date.db-user"),
    password = config.getString("date.db-password")
  )

  override def help(channel: String): OutboundMessage =
    OutboundMessage(channel,
      s"*$name* 處理 editor 相關\\n" +
      "`shelf {user@email.com}` - 顯示用戶 editor 內的書本\\n" +
      "`copy book {12345} to {user@email.com}` - 把 book(12345) 拷貝到 user 的 editor\\n" +
      "`delete book {12345}` - 刪除書本" +
      "`restore order {67890}` - 把訂單(67890) 轉回書本，放回原用戶的 editor\\n" +
      "`restore order {67890} to {user@email.com}` - 把訂單(67890) 轉回書本，放回用戶 user 的 editor\\n" +
      "`restore date {67890}` - 把 Date 訂單(67890) 轉回編輯狀態，放回原用戶的 editor\\n" +
      "`restore date {67890} to {user@email.com}` - 把 Date 訂單(67890) 轉回編輯狀態，放回用戶 user 的 editor\\n" +
      "`json order {67890}` - 把訂單(67890) 輸出 JSON")

  val of = "(?:for|of)".r
  val delete = "(?:rm|removes?|deletes?|kills?)".r
  val restore = "(?:restores?|bookify)".r
  val shelf = "(?:shelf|editor|ls)".r

  val bookID = """(\d+)""".r
  val orderID = """(\d+)""".r
  val email = """<mailto:([^\|]+)\|\1>""".r // email

  val yes = """[Yy][Ee][Ss]\.?""".r

  override def act: Receive = {
    case Command(shelf(), email(user) :: Nil, message) =>
    {
      val books = editorDB.booksForUser(user)
      
      publish(OutboundMessage(message.channel, s"$user 的書架上有 ${books.length} 本書："))
      books.foreach { b => 
        publish(OutboundMessage(message.channel, b.readable))
      }
    }

    case Command("copy", "book" :: bookID(bid) :: "to" :: email(user) :: Nil, message) =>
      editorDB.bookForID(bid.toInt).foreach(book => {
        val newBookID = editorDB.insertBook(book.copy(id = None, user = Some(user)))
        editorDB.bookForID(newBookID).foreach(newBook =>
          publish(OutboundMessage(message.channel, s"已複製: ${newBook.readable}"))
        )
      })

    case Command(delete(), "book" :: bookID(bid) :: Nil, message) =>
      editorDB.bookForID(bid.toInt) match {
        case Some(bookToDelete) => {
          publish(OutboundMessage(message.channel, " 確定要刪除? [yes/no]"))
          publish(OutboundMessage(message.channel, bookToDelete.readable))
          
          context.become {
            case BaseMessage(yes(), channel, message.user, _, _) => {
              editorDB.deleteBookForID(bookToDelete.id.get)
              publish(OutboundMessage(channel, s"已刪除 $bid"))
              context.unbecome()
            }
            case BaseMessage(_, channel, _, _, _) => {
              publish(OutboundMessage(channel, s"取消刪除。"))
              context.unbecome()
            }
            case x => {
              // ignore
            }
          }
        }
        case None => 
          publish(OutboundMessage(message.channel, s"找不到 $bid"))
      }

    case Command(restore(), "order" :: orderID(oid) :: Nil, message) =>
      editorDB.orderForSaleID(oid) match {
        case Some(order) => {
          val book = order.toBook
          editorDB.insertBook(book)
          publish(OutboundMessage(message.channel, s"已放回 $oid"))
        }
        case None =>
          publish(OutboundMessage(message.channel, s"找不到 $oid"))
      }

    case Command(restore(), "order" :: orderID(oid) :: "to" :: email(user) :: Nil, message) =>
      editorDB.orderForSaleID(oid) match {
        case Some(order) => {
          val book = order.toBook.copy(user = Some(user))
          editorDB.insertBook(book)
          publish(OutboundMessage(message.channel, s"已放回 $oid"))
        }
        case None =>
          publish(OutboundMessage(message.channel, s"找不到 $oid"))
      }

    case Command(restore(), "date" :: orderID(oid) :: Nil, message) =>
      dateDB.productForSaleID(oid) match {
        case Some(product) => {
          dateDB.insertProduct(product.restore)
          publish(OutboundMessage(message.channel, s"已放回 $oid"))
        }
        case None =>
          publish(OutboundMessage(message.channel, s"找不到 $oid"))
      }

    case Command(restore(), "date" :: orderID(oid) :: "to" :: email(user) :: Nil, message) =>
      dateDB.productForSaleID(oid) match {
        case Some(product) => {
          var cloned = product.restoreForUser(user)
          dateDB.insertProduct(cloned)
          publish(OutboundMessage(message.channel, s"已放回 $oid"))
        }
        case None =>
          publish(OutboundMessage(message.channel, s"找不到 $oid"))
      }


    case Command("json", "order" :: orderID(oid) :: Nil, message) =>
      editorDB.orderForSaleID(oid) match {
        case Some(order) => {
          val json: String = Json.prettyPrint(order.data.getOrElse(Json.obj()))
          Gist(config.getString("gist-token")).createGist(s"JSON for $oid at ${new java.util.Date}", false, Set(GistFile(s"${oid}.json", json))) match {
            case scala.util.Success(url) ⇒ publish(OutboundMessage(message.channel, s"$oid JSON: $url"))
            case scala.util.Failure(e) ⇒ publish(OutboundMessage(message.channel, e.toString()))
          }
        }
        case None =>
          publish(OutboundMessage(message.channel, s"找不到 $oid"))
      }

  }
}
