namespace TempoTango.Parser

open FParsec
open TempoTango.LinearTimeLogic

module Parser =

  let test p str =
    match run p str with
    | Success(result, _, _)  -> printfn "Succes %A" result
    | Failure(errorMsg, _, _)-> printfn "Failure %s" errorMsg

  let t1 = stringReturn "true" True
  let t2 = stringReturn "T" True
  let t3 = stringReturn "⊤" True

  let f1 = stringReturn "false" False
  let f2 = stringReturn "⊥" False

  let prop = many1Chars( noneOf " \r\n\t!XFG&|UR()∧∨¬⊥⊤T" ) |>> fun s -> Prop s


  let opp = new OperatorPrecedenceParser<expression, unit, unit>()
  let expr = opp.ExpressionParser

  let ws = spaces
  let str_ws s = ws >>. pstring s .>> ws
  let primitive = choice[t1; t2; t3; f1; f2; prop] <|> between (str_ws "(") (str_ws ")") expr
  opp.TermParser <- ws >>. primitive .>> ws

  opp.AddOperator( InfixOperator( "&", ws, 1, Associativity.Left, fun l r -> And( l, r ) ) )
  opp.AddOperator( InfixOperator( "∧", ws, 1, Associativity.Left, fun l r -> And( l, r ) ) )
  opp.AddOperator( InfixOperator( "|", ws, 2, Associativity.Left, fun l r -> Or( l, r ) ) )
  opp.AddOperator( InfixOperator( "∨", ws, 2, Associativity.Left, fun l r -> Or( l, r ) ) )
  opp.AddOperator( InfixOperator( "U", ws, 3, Associativity.Left, fun l r -> Until( l, r ) ) )
  opp.AddOperator( InfixOperator( "R", ws, 3, Associativity.Left, fun l r -> Release( l, r ) ) )

  opp.AddOperator( PrefixOperator("!", ws, 4, true, fun x -> Not( x ) ) )
  opp.AddOperator( PrefixOperator("¬", ws, 4, true, fun x -> Not( x ) ) )
  opp.AddOperator( PrefixOperator("X", ws, 4, true, fun x -> Next( x ) ) )
  opp.AddOperator( PrefixOperator("F", ws, 4, true, fun x -> Finally( x ) ) )
  opp.AddOperator( PrefixOperator("G", ws, 4, true, fun x -> Globally( x ) ) )

  let syntax = expr .>> eof

  let Parse str = match run syntax str with
                    | Success( result, _, _ ) -> result
                    | Failure( err, _, _ ) -> failwith err

  let PrintParse str =
      test syntax str
