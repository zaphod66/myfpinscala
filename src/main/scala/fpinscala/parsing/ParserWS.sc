package fpinscala.parsing

object ParserWS {
  println("Welcome Parser worksheet")             //> Welcome Parser worksheet
  
  import fpinscala.parsing.ParserTypes.Parser
  val P = fpinscala.parsing.ParserImpl            //> P  : fpinscala.parsing.ParserImpl.type = fpinscala.parsing.ParserImpl$@7d401
                                                  //| 2b3
  val json: Parser[JSON] = JSON.jsonParser(P)     //> json  : fpinscala.parsing.ParserTypes.ParseState => fpinscala.parsing.Parser
                                                  //| Types.Result[fpinscala.parsing.JSON] = <function1>

  val wellformedJson1 = """{ "Company Name" : "Actian Corp." }"""
                                                  //> wellformedJson1  : String = { "Company Name" : "Actian Corp." }
  val wellformedJson2 = """{ "Related Companies" : ["HPQ", "IBM", "YHOO", "DELL", "GOOG" ] }"""
                                                  //> wellformedJson2  : String = { "Related Companies" : ["HPQ", "IBM", "YHOO", "
                                                  //| DELL", "GOOG" ] }
  val malformedJson1 = """{ "Company Name" ; "Actian Corp." }"""
                                                  //> malformedJson1  : String = { "Company Name" ; "Actian Corp." }
  val malformedJson2 = """{
    "Company Name"      : "Microsoft Corp.",
    "Ticker"            : "MSFT",
    "Related Companies" : ["HPQ", "IBM", "YHOO" + "DELL", "GOOG" ]
  }"""                                            //> malformedJson2  : String = {
                                                  //|     "Company Name"      : "Microsoft Corp.",
                                                  //|     "Ticker"            : "MSFT",
                                                  //|     "Related Companies" : ["HPQ", "IBM", "YHOO" + "DELL", "GOOG" ]
                                                  //|   }



  P.run(json)(wellformedJson1)                    //> res0: Either[fpinscala.parsing.ParseError,fpinscala.parsing.JSON] = Right(JO
                                                  //| bject(Map(Company Name -> JString(Actian Corp.))))
  P.run(json)(wellformedJson2)                    //> res1: Either[fpinscala.parsing.ParseError,fpinscala.parsing.JSON] = Right(JO
                                                  //| bject(Map(Related Companies -> JArray(Vector(JString(HPQ), JString(IBM), JSt
                                                  //| ring(YHOO), JString(DELL), JString(GOOG))))))
  P.run(json)(malformedJson1)                     //> res2: Either[fpinscala.parsing.ParseError,fpinscala.parsing.JSON] = Left(1.1
                                                  //|  object
                                                  //| 1.18 ':'
                                                  //| 
                                                  //| { "Company Name" ; "Actian Corp." }
                                                  //|                  ^)
  P.run(json)(malformedJson2)                     //> res3: Either[fpinscala.parsing.ParseError,fpinscala.parsing.JSON] = Left(1.1
                                                  //|  object
                                                  //| 4.27 array
                                                  //| 4.49 ']'
                                                  //| 
                                                  //|     "Related Companies" : ["HPQ", "IBM", "YHOO" + "DELL", "GOOG" ]
                                                  //|                                                 ^)
}