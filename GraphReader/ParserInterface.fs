namespace GraphReader

open GraphLexerDef
open SyntAnalysis
open SyntAnalysisDef

module public ParserInterface = 

    let parseFile (filePath:string) =
        let tokens = GraphLexer.getTokensFromFile filePath
        in 
            if validate tokens SyntAnalysisDef.validatorFunc   
            then 
                let (_,con) = buildEdgesList tokens 
                in  Some <|
                    List.fold (fun acc ele ->
                        match ele with
                        | ExtInfo(_,Some(name1),_),ExtInfo(_,Some(name2),_) -> (name1,name2)::acc
                        | _ -> acc 
                    ) [] con
            else None