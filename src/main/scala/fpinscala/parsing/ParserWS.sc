package fpinscala.parsing

object ParserWS {
  println("Welcome Parser worksheet")             //> Welcome Parser worksheet
  
  import fpinscala.parsing.ParserTypes.Parser
  val P = fpinscala.parsing.ParserImpl            //> P  : fpinscala.parsing.ParserImpl.type = fpinscala.parsing.ParserImpl$@38d06
                                                  //| 0ac
  val json: Parser[JSON] = JSON.jsonParser(P)     //> json  : String => fpinscala.parsing.ParserTypes.Result[fpinscala.parsing.JSO
                                                  //| N] = <function1>
}