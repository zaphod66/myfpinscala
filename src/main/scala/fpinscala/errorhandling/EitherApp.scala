package fpinscala.errorhandling

case class Person(name: String, age: Int)
sealed class Name(val value: String)
sealed class Age(val value: Int)
  
object EitherApp extends App {
  def mkName(name: String): Either[String,Name] =
    if (name == null || name == "") Left("name is empty")
    else Right(new Name(name))
    
  def mkAge(age: Int): Either[String,Age] =
    if (age < 0 || age > 120) Left("age is out of Range")
    else Right(new Age(age))
    
  def mkPerson(name: String, age: Int): Either[String,Person] =
    mkName(name).map2(mkAge(age))((n,a) => Person(n.value,a.value))
  
  println(mkPerson("Hans", 42))
  println(mkPerson("",42))
  println(mkPerson("Old", 121))
  println(mkPerson("",121))
}