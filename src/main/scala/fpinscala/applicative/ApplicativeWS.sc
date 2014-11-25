package fpinscala.applicative

import Applicative._
import Monad._

import java.util.Date

object ApplicativeWS {
  val e = eitherMonad[Int]                        //> e  : fpinscala.applicative.Monad[[x]scala.util.Either[Int,x]] = fpinscala.ap
                                                  //| plicative.Monad$$anon$1@7fe9262d
  val u1 = e.unit(1)                              //> u1  : scala.util.Either[Int,Int] = Right(1)
  val u2 = e.flatMap(u1)(i => e.unit(i + 1))      //> u2  : scala.util.Either[Int,Int] = Right(2)
 
  case class Form(name: String, date: Date)
  
  def validName(name: String): Validation[String,String] =
    if (name != "") Success(name)
    else Failure("<name> cannot be empty")        //> validName: (name: String)fpinscala.applicative.Validation[String,String]

  def validDate(date: String): Validation[String, Date] =
    try {
      import java.text._
      
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(date))
    } catch {
      case _ : Throwable => Failure("date must be of format yyyy-MM-dd")
    }                                             //> validDate: (date: String)fpinscala.applicative.Validation[String,java.util.D
                                                  //| ate]
  def validForm(name: String, date: String) = {
    val v = validationApplicative[String]
    v.map2(validName(name),validDate(date))(Form(_,_))
  }                                               //> validForm: (name: String, date: String)fpinscala.applicative.Validation[Stri
                                                  //| ng,fpinscala.applicative.ApplicativeWS.Form]
  validForm("Jana","1976-11-07")                  //> res0: fpinscala.applicative.Validation[String,fpinscala.applicative.Applicat
                                                  //| iveWS.Form] = Success(Form(Jana,Sun Nov 07 00:00:00 CET 1976))
  validForm("","--")                              //> res1: fpinscala.applicative.Validation[String,fpinscala.applicative.Applicat
                                                  //| iveWS.Form] = Failure(<name> cannot be empty,Vector(date must be of format y
                                                  //| yyy-MM-dd))
}