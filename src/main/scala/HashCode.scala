import HashCode.totalDays

import scala.io
import scala.io.{BufferedSource, Source}

object HashCode extends App {
  val src: BufferedSource = Source.fromResource("e_so_many_books.txt")

  val indexedlines: Array[String] = src.getLines().toArray

  val firstLine = indexedlines(0).split(' ')
  val totalBooks = firstLine(0)
  val totalLibs = firstLine(1)
  val totalDays = firstLine(2).toInt

  val booksMap: Map[Int, Book] = indexedlines(1).split(' ').zipWithIndex.map {
    case (score, index) => Book(index, score.toInt)
  }.map(b => b.id -> b).toMap

  val libraries: Set[Library] = indexedlines.drop(2).grouped(2).filter(_.length == 2).zipWithIndex.map {
    case (libLine, id) =>
      val libInfo = libLine(0).split(' ')
      val signupTime = libInfo(1)
      val booksperDay = libInfo(2)
      val bookLine = libLine(1).split(' ')
      val books = bookLine.map(id => booksMap(id.toInt)).toSet
      Library(id, books, signupTime.toInt, booksperDay.toInt)
  }.toSet

  val result = Scoring.chooseLibrary(libraries, totalDays, List.empty[Int])
  println(result)
}

object Scoring {
  def scoreLibrary(library: Library, daysLeft: Int): Int = {
    val books: List[List[Book]] = library.books.toList.sortBy(_.score).grouped(library.booksPerDay).toList
    books.take(daysLeft - library.signupTime).map {
      group => group.map(_.score).sum
    }.sum
  }

  def chooseLibrary(libs: Set[Library], daysLeft: Int, libOrder: List[Int]): List[Int] = {
    if (libs.isEmpty) {
      libOrder
    }
    else {
      val nextLib = libs.toList.map {
        lib => lib -> Scoring.scoreLibrary(lib, daysLeft)
      }.sortBy(_._2).reverse.head
      chooseLibrary(libs - nextLib._1, daysLeft - nextLib._1.signupTime, (libOrder.+:(nextLib._1.id)))
    }
  }
}


case class Library(id: Int, books: Set[Book], signupTime: Int, booksPerDay: Int, var processed: Boolean = false)
case class Book(id: Int, score: Int)