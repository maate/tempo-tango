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

  let apply f p =
    parse
        { let! v = p
          return f v }

  let op1 = (anyOf "!XFG") .>>. expr |> apply ( fun (c, e) -> match c with
                                                              | '!' -> Not e
                                                              | 'X' -> Next e
                                                              | 'F' -> Finally e
                                                              | 'G' -> Globally e
                                                              | _   -> failwith "err" );

  let varOp2 = parse {
    let! first = primitive
    let! res = 
      choice [ (anyOf "&|UR") .>>. expr |> apply (fun ( c , e ) -> match c with 
                                                                           | '|' -> Or(first, e)
                                                                           | '&' -> And(first, e)
                                                                           | _   -> failwith "err" )
               parse { return first } ]
               
    return res }

  do exprImpl := spaces >>. choice[op1;varOp2]
  
  let syntax = expr

  let Parse str = match run syntax str with
                    | Success( result, _, _ ) -> result
                    | Failure( err, _, _ ) -> failwith err

  let PrintParse str =
      test syntax str
