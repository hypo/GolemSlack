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

  override def help(channel: String): OutboundMessage =
    OutboundMessage(channel,
      s"$name hypo. \\n" +
      "Usage: TODO")

  val of = "(?:for|of)".r
  val delete = "(?:rm|removes?|deletes?|kills?)".r
  val restore = "(?:restores?|bookify)".r

  val bookID = """(\d+)""".r
  val orderID = """(\d+)""".r
  val email = """<mailto:([^\|]+)\|\1>""".r // email

  override def act: Receive = {
    case Command("editor", "shelf" :: of() :: email(user) :: Nil, message) =>
    {
      val books = editorDB.booksForUser(user)
      
      publish(OutboundMessage(message.channel, s"$user 的書架上有 ${books.length} 本書："))
      books.foreach { b => 
        publish(OutboundMessage(message.channel, b.readable))
      }
    }

    case Command("editor", "book" :: "copy" :: bookID(bid) :: "to" :: email(user) :: Nil, message) =>
      editorDB.bookForID(bid.toInt).foreach(book => {
        val newBookID = editorDB.insertBook(book.copy(id = None, user = Some(user)))
        editorDB.bookForID(newBookID).foreach(newBook =>
          publish(OutboundMessage(message.channel, s"已複製: ${newBook.readable}"))
        )
      })

    case Command("editor", "book" :: delete() :: bookID(bid) :: Nil, message) =>
      val response = OutboundMessage(message.channel, s"delete $bid")
      publish(response)

    case Command("editor", "order" :: restore() :: orderID(oid) :: Nil, message) =>
      val response = OutboundMessage(message.channel, s"restore $oid")
      publish(response)

    case Command("editor", "order" :: restore() :: orderID(oid) :: "to" :: email(user) :: Nil, message) =>
      val response = OutboundMessage(message.channel, s"restore $oid to $user")
      publish(response)

    case Command("editor", "order" :: "json" :: orderID(oid) :: Nil, message) =>
      val response = OutboundMessage(message.channel, s"json")
      publish(response)

  }
}