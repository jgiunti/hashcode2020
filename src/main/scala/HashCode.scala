import scala.io
import scala.io.{BufferedSource, Source}

object HashCode extends App {
  val src: BufferedSource = Source.fromResource("a_example.txt")

  val indexedlines: Array[String] = src.getLines().toArray

  val firstLine = indexedlines(0).split(' ')
  val totalBooks = firstLine(0)
  val totalLibs = firstLine(1)
  val totalDays = firstLine(2)

  val booksMap: Map[Int, Book] = indexedlines(1).split(' ').zipWithIndex.map {
    case (score, index) => Book(index, score.toInt)
  }.map(b => b.id -> b).toMap

  val libLines: Seq[Library] = indexedlines.drop(2).grouped(2).map {
    libLine =>
      val libInfo = libLine(0).split(' ')
      val signupTime = libInfo(1)
      val booksperDay = libInfo(2)
      val bookLine = libLine(1).split(' ')
      val books = bookLine.map(id => booksMap(id.toInt)).toSet
      Library(books, signupTime.toInt, booksperDay.toInt)
  }.toList

  
}

case class Library(books: Set[Book], signupTime: Int, booksPerDay: Int)
case class Book(id: Int, score: Int)