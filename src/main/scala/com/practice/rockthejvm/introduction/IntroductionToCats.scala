package com.practice.rockthejvm.introduction

//noinspection ScalaWeakerAccess,ScalaUnusedSymbol,TypeAnnotation
object IntroductionToCats {

  // the general pattern of using a type class in cats

  // part 1 - import type class
  import cats.Eq

  // part 2 - import type class instances for the types you need
  import cats.instances.int._

  // part 3 - use the type class API
  val intEquality         = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false
  // val anUnsafeComparison = intEquality.eqv(2, "a string") -- must have the same type!

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComp = 2 === 3 // eqv(2, 3) false
  val neqComparison       = 2 =!= 3 // neqv(2, 3) true
  // val invalidComparison = 2 === "a String" -- must have the same type!

  // part 5 - extending the type class operations to composite types, e.g lists
  //          extension methods are only visible in the presence of the right type class instance
  import cats.instances.list._
  val aListComparison = List(2) === List(3)

  // part 6 - create a type class instance for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price === car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99)

  def main(args: Array[String]): Unit = {
    println(compareTwoToyCars)
  }
}
