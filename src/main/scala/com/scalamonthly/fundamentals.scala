package com.scalamonthly

import java.util.UUID
import monocle.macros.GenLens
import monocle.std.option.some
import com.scalamonthly.fundamentals.Animal.DogName
import monocle.Prism
import com.scalamonthly.fundamentals.Animal.Dog
import monocle.function.Plated
import monocle.Traversal
import cats.implicits._
import com.scalamonthly.fundamentals.Tree.Branch
import com.scalamonthly.fundamentals.Tree.Leaf

object fundamentals {

  final case class FirstName(value: String) extends AnyVal
  final case class LastName(value: String) extends AnyVal
  final case class Name(firstName: FirstName, lastName: LastName)
  final case class Person(id: UUID, name: Name)
  final case class AccountHolder(primary: Person, secondary: Option[Person])
  final case class BankAccount(id: UUID, holder: AccountHolder, balance: Long)

  private val primaryFirstName = GenLens[BankAccount](_.holder.primary.name.firstName)

  /**
    * Return the firstName of the primary account holder.
    */
  def one(ba: BankAccount): FirstName = {
    primaryFirstName.get(ba)
  }

  /**
    * Update the firstName of the primary account holder to be "Mal"
    */
  def two(ba: BankAccount): BankAccount = {
    primaryFirstName.set(FirstName("Mal"))(ba)
  }

  private val accountHolder = GenLens[BankAccount](_.holder)
  private val primary = GenLens[AccountHolder](_.primary)
  private val secondary = GenLens[AccountHolder](_.secondary)
  private val lastName = GenLens[Person](_.name.lastName)

  /**
    * Update the lastName of the primary and secondary (if exists) account holders to be "Fischer"
    */
  def three(ba: BankAccount): BankAccount = {
    val updatedPrimary = accountHolder.composeLens(primary).composeLens(lastName).set(LastName("Fischer"))
    val updatedSecondary = accountHolder.composeLens(secondary).composePrism(some).composeLens(lastName).set(LastName("Fischer"))
    updatedPrimary.compose(updatedSecondary)(ba)
  }

  sealed abstract class Animal extends Product with Serializable
  object Animal {
    final case class DogName(value: String) extends AnyVal
    final case class Dog(name: DogName) extends Animal
    final case class Fish(length: Int) extends Animal
    final case class Turtle(age: Int) extends Animal
  }

  private val dogPrism = Prism[Animal, DogName] {
    case Dog(name) => Some(name)
    case _ => None
  }(name => Dog(name))

  /**
    * Use a Prism to return the name of the animal if it is a dog, else return None.
    */
  def four(a: Animal): Option[DogName] = {
    dogPrism.getOption(a)
  }

  /**
    * Use a Prism to construct an Animal given a DogName.
    */
  def five(d: Animal.DogName): Animal = {
    dogPrism(d)
  }

  final case class HouseholdOccupants(owner: Person, pet: Animal)
  final case class Household(id: UUID, occupants: HouseholdOccupants)

  private val animalLens = GenLens[Household](_.occupants.pet)
  
  /**
    * If the household pet is a dog, append " II" to its name and return the entire household.
    */
  def six(h: Household): Household = {
    animalLens.composePrism(dogPrism).modify(dn => Animal.DogName(dn.value + " II"))(h)
  }

  sealed abstract class Tree[+A] extends Product with Serializable
  object Tree {
    final case class Branch[A](data: A, left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](data: A) extends Tree[A]
  }

  private implicit def treePlated[A]: Plated[Tree[A]] = Plated(
    new Traversal[Tree[A],Tree[A]] {
      def modifyF[F[_]: cats.Applicative](f: Tree[A] => F[Tree[A]])(s: Tree[A]): F[Tree[A]] = s match {
        case Tree.Leaf(d) => Tree.Leaf(d).pure[F].widen
        case Tree.Branch(d, l, r) => cats.Applicative[F].product(f(l), f(r))
          .map(res => Tree.Branch(d, res._1, res._2))
      }
    }
  )

  /**
    * Add 1 to every leaf node inside of the tree.
    * 
    * Hint: Look at the Plated type-class from Monocle.
    */
  def seven(tree: Tree[Int]): Tree[Int] = {
    Plated.transform[Tree[Int]] {
      case b: Branch[Int] => b
      case Leaf(data) => Leaf(data + 1)
    }(tree)
  }

}