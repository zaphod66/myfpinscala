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
  }
  """
  
  val malformedJSON1 = """
  {
    "Company Name" ; "Microsoft Corp."
  }
  """

  def printResult[E](e: Either[E,JSON]) = e.fold(println, println)
  
  import fpinscala.parsing.ParserTypes.Parser
  val P = fpinscala.parsing.ParserImpl
  val json: Parser[JSON] = JSON.jsonParser(P)
  
  printResult { P.run(json)(jsonTxt) }
}