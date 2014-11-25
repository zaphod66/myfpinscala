package fpinscala.applicative

import Applicative._
import Monad._

import java.util.Date

object ApplicativeApp extends App {
  val e = eitherMonad[Int]

  val u1 = e.unit(1)
  val u2 = e.flatMap(u1)(i => e.unit(i+1))
  val u3 = e.flatMap(u2)(i => Left(-1))
  
  println(u1)
  println(u2)
  println(u3)
  
  println("===========")

  case class Form(name: String, date: Date, number: String)
  
  def validName(name: String): Validation[String,String] = 
    if (name != "") Success(name)
    else Failure("name cannot be emtpy")
  
  def validDate(date: String): Validation[String, Date] =
    try {
      import java.text._
      
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(date))
    } catch {
      case _ : Throwable => Failure("date must be in format yyyy-MM-dd")
    }
  
  def validPhone(number: String): Validation[String,String] =
    if (number.matches("[0-9]{10}"))
      Success(number)
    else
      Failure("Phone number must be 10 digits long")
  
  def validForm(name: String, date: String, number: String) = {
    val v = validationApplicative[String]
    v.map3(validName(name), validDate(date), validPhone(number))(Form(_,_,_))
  }
  
  val form1 = validForm("Jana", "1976-11-07", "0123456789")
  val form2 = validForm("","-11-07", "+4915140235260")
  
  println(form1)
  println(form2)
}
