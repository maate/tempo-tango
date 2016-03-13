namespace TempoTango.Parser

open FParsec
open TempoTango.LinearTimeLogic

module Parser =

  let test p str =
    match run p str with
    | Success(result, _, _)  -> printfn "Succes %A" result
    | Failure(errorMsg, _, _)-> printfn "Failure %s" errorMsg

  let t = stringReturn "true"  True
  let f = stringReturn "false" False
  let prop = many1Chars( noneOf " !XFG&|UR()" ) |>> fun s -> Prop s


  let opp = new OperatorPrecedenceParser<expression, unit, unit>()
  let expr = opp.ExpressionParser

  let str_ws s = spaces .>>. pstring s
  let primitive = choice[t; f; prop] <|> between (str_ws "(") (str_ws ")") expr
  opp.TermParser <- primitive

  opp.AddOperator( InfixOperator( "&", spaces, 1, Associativity.Left, fun l r -> And( l, r ) ) )
  opp.AddOperator( InfixOperator( "|", spaces, 2, Associativity.Left, fun l r -> Or( l, r ) ) )
  opp.AddOperator( InfixOperator( "U", spaces, 3, Associativity.Left, fun l r -> Until( l, r ) ) )
  opp.AddOperator( InfixOperator( "R", spaces, 3, Associativity.Left, fun l r -> Release( l, r ) ) )

  opp.AddOperator( PrefixOperator("!", spaces, 4, true, fun x -> Not( x ) ) )
  opp.AddOperator( PrefixOperator("X", spaces, 4, true, fun x -> Next( x ) ) )
  opp.AddOperator( PrefixOperator("F", spaces, 4, true, fun x -> Finally( x ) ) )
  opp.AddOperator( PrefixOperator("G", spaces, 4, true, fun x -> Globally( x ) ) )

  let syntax = expr

  let Parse str = match run syntax str with
                    | Success( result, _, _ ) -> result
                    | Failure( err, _, _ ) -> failwith err

  let PrintParse str =
      test syntax str
