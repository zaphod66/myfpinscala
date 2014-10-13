package fpinscala.parsing

import scala.language.implicitConversions
import scala.language.higherKinds

trait JSON

object JSON {
  case object JNULL extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String,JSON]) extends JSON
  
  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))
    
    def obj = surround("{","}")(succeed(JNULL))
    
    root(whitespace *> obj)
  }
}
