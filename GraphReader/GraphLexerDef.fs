namespace GraphReader

module internal GraphLexerDef = 
 
    type GraphTokens = 
        | StartToken      of int * string * int
        | EmptyStartToken of int * int
        | EmptyEndToken   of int * int
        | EndToken        of int * string

    let separateToken                = "-->"
    let getStartPos (str:string) pos = pos+str.IndexOf(separateToken)
    let getEndPos (str:string)   pos = 
        if str.LastIndexOf(separateToken) <> str.IndexOf(separateToken)
        then pos + str.LastIndexOf(separateToken)
        else pos + str.Length
    let extractLabel (str:string) = 
        str.Substring(
            str.IndexOf('{') + 1,
                str.IndexOf('}') - str.IndexOf('{') - 1
        )

    let RegexMapping = [
        ("\s*-->\s*{\w+}\s*-->",
            fun pos (str:string) -> 
                StartToken (
                    getStartPos str pos,
                    extractLabel str,
                    getEndPos str pos),
                getEndPos str pos);

        ("-->\s+-->",
            fun pos (str:string) -> 
                EmptyStartToken (
                    getStartPos str pos,
                    getEndPos str pos),
                getEndPos str pos);

        ("\s*-->\s*{\w+}\s*$", 
            fun pos (str:string) -> 
                EndToken (
                    getStartPos str pos,
                    extractLabel str),
                getEndPos str pos);

        ("(-->\s*{}\s*-->|-->\s*{}\s*$)", 
            fun pos (str:string) -> 
                EmptyEndToken (
                    getStartPos str pos,
                    getEndPos str pos),
                getEndPos str pos)
    ]

