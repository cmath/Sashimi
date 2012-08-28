namespace GraphReader

open GraphLexerDef
open GraphLexer
open SyntAnalysisDef

module internal SyntAnalysis = 

    let validate tokens validators = 
        List.fold (fun _ (validator,excp) -> 
            if validator <| tokens
            then true
            else raise excp
        ) true validators 

    let rec findConnected token parsedTokens = 
        match token with
        | StartToken(0,_,_) 
        | EmptyStartToken _ -> None
        | EndToken(n,_)
        | StartToken(n,_,_) ->
            match parsedTokens with
            | []     -> None
            | hd::tl ->
                match hd with
                | StartToken(_,_,m)  when n=m -> Some(hd,token)
                | EmptyEndToken(_,m) when n=m -> 
                    match findConnected hd tl with 
                    | Some(result,_) -> Some(result,token)
                    | _              -> raise <| System.Exception("Graph is malformed!!!") 
                | _ -> findConnected token tl
        | EmptyEndToken(n,m) -> 
                match parsedTokens with
                | []     -> None
                | (EndToken(m,_) as hd)     :: _             -> Some(hd,token)
                | (StartToken(n,_,_) as hd) :: tl when n = m -> Some(hd,token)
                | (StartToken(n,_,m) as hd) :: tl            -> Some(hd,token) 
                | EmptyStartToken(n,_) :: (StartToken(_,_,m) as startToken) :: tl 
                    when n=m ->
                        match findConnected token tl with 
                        | Some(result,_) -> Some(result,startToken)
                        | _              -> raise <| System.Exception("Graph is malformed!!!") 
                | _::tl -> findConnected token tl

    let buildEdgesList tokens = 
        Seq.fold (fun (parsed,connected) ele ->
            match findConnected ele parsed with 
            | None      -> (ele::parsed,connected)
            | Some(con) -> (ele::parsed,con::connected)
        ) ([],[]) tokens
