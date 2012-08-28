// Learn more about F# at http://fsharp.net

let (Some(tokens)) = GraphReader.ParserInterface.parseFile "D:/test.txt"
List.iter (fun (ele1,ele2) -> printf "%s" ((ele1.ToString())+"<--->"+(ele2.ToString())+"\n")) tokens 