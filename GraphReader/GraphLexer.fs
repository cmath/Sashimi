namespace GraphReader

open System.IO
open System.Text.RegularExpressions
open GraphLexerDef

module internal GraphLexer =

    let prettyPrint token = 
        match token with
        | StartToken (num,str,num2) -> "start-"       + num.ToString() + "-" + str + "-" + num2.ToString()
        | EmptyStartToken (num,num2)-> "empty_start-" + num.ToString() + "-" + num2.ToString()
        | EmptyEndToken (num,num2)  -> "empty_end-"   + num.ToString() + "-" + num2.ToString()
        | EndToken (num,str)        -> "end-"         + str            + "-" + num.ToString()

    let readFile path = seq {
        use streamReader = new StreamReader (path:string)
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine ()
    }

    let parseLine (str:string) regexPatterns =  
        let rec subParseLine (subStr:string) subPatterns offset =  
            match subPatterns with
            |[]             -> [] 
            | (pt,func)::tl -> 
                let matched = Regex.Match(subStr, pt) in
                    if matched.Success && matched.Index = 0
                    then let (token,endPos) = func (offset + matched.Index) matched.Value in 
                            token::(subParseLine (subStr.Substring(endPos - offset)) regexPatterns endPos)
                    else subParseLine (subStr:string) tl offset
        subParseLine str regexPatterns 0

    let getTokensFromFile (filePath:string) =
        Seq.fold (fun acc (ele:string) -> acc@(parseLine ele RegexMapping) ) [] (readFile filePath)

 
