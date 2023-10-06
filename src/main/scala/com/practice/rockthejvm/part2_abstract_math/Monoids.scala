package com.practice.rockthejvm.part2_abstract_math

//noinspection ScalaWeakerAccess,TypeAnnotation,ScalaUnusedSymbol
object Monoids {

  // import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| extension method
  val numbers = (1 to 1000).toList
  // |+| is always associative
  // from left to right or from right to left would always produce the same result
  val sumLeft  = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  // def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  //   list.foldLeft(/* WHAT?! */)(_ |+| _)

  // MONOIDS provides neutral value
  import cats.Monoid
  val intMonoid  = Monoid[Int]
  val combineInt = intMonoid.combine(25, 999) // 1024
  val zero       = intMonoid.empty            // 0

  import cats.instances.string._ // bring the implicit Monoid[String] in scope
  val emptyString   = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._
  val emptyOption    = Monoid[Option[Int]].empty                                 // None
  val combineOption  = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6))         // Some(8)

  // extension methods for Monoids - |+|
  // import cats.syntax.monoid._ // either this one or cats.syntax.semigroup._
  val combinedOptionFancy = Option(3) |+| Option(7)

  // Excercise 1: implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // Exercise 2: combine a list of phonebooks as Maps[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob"   -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel"  -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.map._
  val massivePhonebook = combineFold(phonebooks)

  // Exercise 3: shopping cart and online stores with Monoids
  case class ShoppingCart(items: List[String], total: Double)
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List(), 0.0),
    (x, y) => ShoppingCart(x.items ++ y.items, x.total + y.total)
  )
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    // test exercise 1
    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "monoids")))

    // test exercise 2
    println(massivePhonebook)

    // test exercise 3
    println(
      checkout(
        List(
          ShoppingCart(List("iphone", "shoes"), 799),
          ShoppingCart(List("TV"), 20000),
          ShoppingCart(List(), 0)
        )
      )
    )
  }
}
