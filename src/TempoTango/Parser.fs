namespace M8.TempoTango

open FParsec
open M8.TempoTango.LinearTemporalLogic

module internal Parser =

  let test p str =
    match run p str with
    | Success(result, _, _)  -> printfn "Succes %A" result
    | Failure(errorMsg, _, _)-> printfn "Failure %s" errorMsg

  let t1 = stringReturn "true" True
  let t2 = stringReturn "T" True
  let t3 = stringReturn "⊤" True

  let f1 = stringReturn "false" False
  let f2 = stringReturn "⊥" False

  let prop = many1Chars( choice[lower;digit] ) |>> fun s -> Prop s

  let empty1 = stringReturn "E" Empty
  let empty2 = stringReturn "ε" Empty

  let opp = new OperatorPrecedenceParser<expression, unit, unit>()
  let expr = opp.ExpressionParser

  let ws = spaces
  let str_ws s = ws >>. pstring s .>> ws
  let primitive = choice[t1; t2; t3; f1; f2; empty1; empty2; prop] <|> between (str_ws "(") (str_ws ")") expr
  opp.TermParser <- ws >>. primitive .>> ws

  opp.AddOperator( PrefixOperator( "!", ws, 100, true, fun x -> Not( x ) ) )
  opp.AddOperator( PrefixOperator( "¬", ws, 100, true, fun x -> Not( x ) ) )
  opp.AddOperator( PrefixOperator( "X", ws, 100, true, fun x -> Next( x ) ) )
  opp.AddOperator( PrefixOperator( "F", ws, 100, true, fun x -> Finally( x ) ) )
  opp.AddOperator( PrefixOperator( "G", ws, 100, true, fun x -> Globally( x ) ) )

  opp.AddOperator( InfixOperator( "&", ws, 10, Associativity.Left, fun l r -> And( l, r ) ) )
  opp.AddOperator( InfixOperator( "∧", ws, 10, Associativity.Left, fun l r -> And( l, r ) ) )
  opp.AddOperator( InfixOperator( "U", ws, 9, Associativity.Left, fun l r -> Until( l, r ) ) )
  opp.AddOperator( InfixOperator( "R", ws, 9, Associativity.Left, fun l r -> Release( l, r ) ) )
  opp.AddOperator( InfixOperator( "W", ws, 9, Associativity.Left, fun l r -> WeakUntil( l, r ) ) )
  opp.AddOperator( InfixOperator( "?", ws, 9, Associativity.Left, fun l r -> Optional( l, r ) ) )

  opp.AddOperator( InfixOperator( "|", ws, 8, Associativity.Left, fun l r -> Or( l, r ) ) )
  opp.AddOperator( InfixOperator( "∨", ws, 8, Associativity.Left, fun l r -> Or( l, r ) ) )

  opp.AddOperator( InfixOperator( "->", ws, 5, Associativity.Left, fun l r -> Implicate( l, r ) ) )

  let syntax = expr .>> eof

  let public Parse str = match run syntax str with
                           | Success( result, _, _ ) -> result
                           | Failure( err, _, _ ) -> failwith err

  let PrintParse str =
      test syntax str
