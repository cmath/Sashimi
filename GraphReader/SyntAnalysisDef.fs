namespace GraphReader

open GraphLexerDef
open GraphLexer

module internal SyntAnalysisDef =

    let (|ExtInfo|) ele = 
        match ele with
        | StartToken(pred,label,suc) -> pred,Some(label),Some(suc)
        | EndToken(pred,label)       -> pred,Some(label),None
        | EmptyStartToken(pred,suc)  -> pred,None,Some(suc)
        | EmptyEndToken(pred,suc)    -> pred,None,Some(suc)

    let sematicRules tokens = 
        match tokens with
        |StartToken(0,_,_) :: []
        |StartToken _      :: StartToken    _ :: _ 
        |StartToken _      :: EmptyEndToken _ :: _ 
        |StartToken _      :: EndToken      _ :: _ -> true
        |EndToken(_,_)     :: _ -> true
        |EndToken (pred,_)         :: StartToken(_,_,suc)    :: _ 
        |EmptyEndToken (pred,_)    :: StartToken (_,_,suc)   :: _ 
        |EmptyEndToken (pred,_)    :: EmptyStartToken(_,suc) :: _
        |EmptyStartToken (pred,_)  :: StartToken (_,_,suc)   :: _ -> pred = suc
        |_ -> false 

    let validatorFunc:list<(list<GraphTokens>->bool)*System.Exception> = [
       ((fun tokens -> 
            List.filter (fun ele -> 
                match ele with 
                | StartToken (0,_,_) -> true
                | _                  -> false
            ) tokens |> Seq.length = 1),
            System.Exception("Must have one start!!"));
       ((fun tokens -> 
            List.filter (fun ele -> 
                match ele with 
                | EndToken (_,_) -> true
                | _              -> false
            ) tokens |> Seq.length  > 0),
            System.Exception("Must have end!!"));

       ((fun tokens ->
            let keyFun ele = 
                match ele with
                | StartToken(_,label,_) -> Some(label) 
                | EndToken(_,label)     -> Some(label)
                | _                     -> None
            in 
            Seq.fold (fun acc (key,arity) -> 
                match key with
                | Some(_) -> arity < 2
                | _       -> acc 
            ) true <| Seq.countBy keyFun (tokens:> seq<GraphTokens>)),
            System.Exception("Labels must be unique!!"));

        ((fun tokens ->
            List.fold ( 
                fun acc (ExtInfo(pred,_,_) as ele) -> 
                    acc && ( pred = 0  
                            || Seq.exists ( fun (ExtInfo(_,_,ssuc) as sele)  ->
                                ele <> sele && ssuc = Some(pred)
                            ) tokens ) 
             ) true tokens),
             System.Exception("Token must have parent!!"))
    ]





