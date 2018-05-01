case class Book(title: String, authors: List[String])

val books = Set(
  Book(
    title = "Structure and Interpretation of Computer Programs",
    authors = List("Adelson, Harald", "Sussman, Gerald J.")),
  Book(
    title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(
    title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(
    title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(
    title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon Lex", "Venners, Bill")),
  Book(
    title = "More Effective Java",
    authors = List("Bloch, Joshua"))
)

for (book <- books; author <- book.authors if author startsWith "Bloch,")
  yield book.title


for {
  book1 <- books
  book2 <- books
  if book1.title < book2.title
  author1 <- book1.authors
  author2 <- book2.authors
  if author1 == author2
} yield author1