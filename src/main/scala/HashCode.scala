import HashCode.totalDays

import scala.io
import scala.io.{BufferedSource, Source}

object HashCode extends App {
  val src: BufferedSource = Source.fromResource("a_example.txt")

  val indexedlines: Array[String] = src.getLines().toArray

  val firstLine = indexedlines(0).split(' ')
  val totalBooks = firstLine(0).toInt
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

  val re = List("1", "2", "3")

  //589 22490
  val result = Scoring.chooseLibrary(totalBooks, libraries, totalDays, List.empty[(Int, List[Int])])
  val r =  result.map {
    case (libId, bookIds) if bookIds.nonEmpty => s"$libId ${bookIds.size}\n" + bookIds.map(id => s"$id ").reduce((a, b) => a+b) + "\n"
  }.reduce((a, b) => a+b)

  val resString = s"${result.size}\n" + r
  println(resString)
}

object Scoring {
  def scoreLibrary(library: Library, daysLeft: Int, excludeBooks: Set[Book]): (Int, List[Book]) = {
    val sortedBooks: List[List[Book]] = library.books.diff(excludeBooks).toList.sortBy(_.score).grouped(library.booksPerDay).toList
    val booksToProcess = sortedBooks.take(daysLeft - library.signupTime)
    val score = booksToProcess.take(daysLeft - library.signupTime).map {
      group => group.map(_.score).sum
    }.sum
    (score, booksToProcess.flatten)
  }

  def chooseLibrary(totalBooks: Int, libs: Set[Library], daysLeft: Int, libOrder: List[(Int, List[Int])], seenBooks: Set[Book] = Set.empty[Book]): List[(Int, List[Int])] = {
    if (libs.isEmpty || daysLeft == 0 || totalBooks == seenBooks.size) {
      libOrder
    }
    else {
      val nextLib = libs.toList.map {
        lib => lib -> Scoring.scoreLibrary(lib, daysLeft, seenBooks)
      }.sortBy(_._2._1).reverse.head
      chooseLibrary(totalBooks, libs - nextLib._1, daysLeft - nextLib._1.signupTime, libOrder.:+((nextLib._1.id, nextLib._2._2.map(_.id))), seenBooks ++ nextLib._2._2)
    }
  }
}


case class Library(id: Int, books: Set[Book], signupTime: Int, booksPerDay: Int, var processed: Boolean = false)
case class Book(id: Int, score: Int)