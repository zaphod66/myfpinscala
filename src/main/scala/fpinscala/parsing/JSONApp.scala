package fpinscala.parsing

object JSONApp extends App {
  val jsonTxt = """
    {
      "Company name" : "Microsoft Corp.",
      "Ticker"       : "MSFT",
      "Active"       : true,
      "Price"        : 30.66,
      "Shares outstanding" : 8.38e9,
      "Related Companies" : ["HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
    }"""
  
  val malformedJson1 = """
    {
      "Company Name" ; "Microsoft Corp."
    }"""


  val malformedJson2 = """
    {
      "Company Name" : "Microsoft Corp.",
      "Ticker"       : "MSFT",
      "Related" : ["HPQ", "IBM", "YHOO", "DELL" ++ "GOOG"]
    }"""
    
  def printResult[E](e: Either[E,JSON]) = e.fold(println, println)
  
  import fpinscala.parsing.ParserTypes.Parser
  val P = fpinscala.parsing.ParserImpl
  val json: Parser[JSON] = JSON.jsonParser(P)
  
  printResult { P.run(json)(jsonTxt) }
  println("++++++")
  printResult { P.run(json)(malformedJson1) }
  println("++++++")
  printResult { P.run(json)(malformedJson2) }
}