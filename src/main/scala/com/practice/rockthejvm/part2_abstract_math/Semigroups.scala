package com.practice.rockthejvm.part2_abstract_math

//noinspection ScalaWeakerAccess,TypeAnnotation
object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination      = naturalIntSemigroup.combine(2, 46) // addition
  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination      = naturalStringSemigroup.combine("I love ", "Cats") // concatenation

  // sepecific API
  def reduceInts(list: List[Int]): Int          = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  // Excercise 1: support a new type
  // hint: use the same pattern we used with Eq
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (e1, e2) =>
    Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum         = 2 |+| 3 // require the presence of an implicit Semigroup[Int]
  val aStringConcat    = "we like " |+| "semigroup"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  // Exercise 2: implement recudeThings2 with the |+|
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers)) // compiler injects the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler injects the implicit Semigroup[String]
    import cats.instances.option._
    // compiler will produce an implicit Semigroup[Option[Int]] - combine will produce antoher option with summed elements
    // compiler will produce an implicit Semigroup[Option[String]] - combine will produce another option with the concatenated elements
    val numberOptions: List[Option[Int]] =
      numbers.map(Option(_)) // an Option[Int] containing the sum of the numbers
    println(reduceThings(numberOptions))
    val stringOptions: List[Option[String]] = strings.map(Option(_))
    println(reduceThings(stringOptions))

    // test exercise 1
    val expenses = List(Expense(1, 99), Expense(2, 35), Expense(43, 10))
    println(reduceThings(expenses))

    // test exercise 2
    println(reduceThings2(expenses))
  }
}
