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
  let prop = many1Chars( noneOf " !XFG&|UR" ) |>> fun s -> Prop s

  let primitive = choice[t; f; prop]

  let expr, exprImpl = createParserForwardedToRef()

  let expressionOperation1 = anyOf "!XFG"                 >>=? fun operator ->
                             expr                         >>=? fun operand ->
                             preturn operand              |>> ( match operator with
                                                                | '!' -> Not
                                                                | 'X' -> Next
                                                                | 'F' -> Finally
                                                                | 'G' -> Globally
                                                                | _   -> failwith "err" )

//  System.Diagnostics.Debugger.Launch()
  let expressionOperation2 = expr .>>. ( anyOf "&|UR" ) .>>. expr |>> fun ( ( l, op ), r ) ->
                                                                              ( match op with
                                                                                | '&' -> And( l, r )
                                                                                | '|' -> Or( l, r )
                                                                                | 'U' -> Until( l, r )
                                                                                | 'R' -> Release( l, r )
                                                                                | _   -> failwith "err" )
//                           >>=? fun operandL ->
//                             anyOf "&|UR"                 >>=? fun operator ->
//                             primitive                         >>=? fun operandR ->
//                             preturn (operandL, operandR) |>> ( match operator with
//                                                                | '&' -> And
//                                                                | '|' -> Or
//                                                                | 'U' -> Until
//                                                                | 'R' -> Release
//                                                                | _   -> failwith "err" )

  do exprImpl := spaces >>. choice[expressionOperation1; attempt expressionOperation2; primitive] .>> eof
  
  let syntax = expr

  let Parse str = match run syntax str with
                    | Success( result, _, _ ) -> result
                    | Failure( err, _, _ ) -> failwith err

  let PrintParse str =
      test syntax str
