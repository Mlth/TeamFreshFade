// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun c -> if System.Char.IsWhiteSpace c then true else false) <?> "whitespace"
    let pletter        = satisfy (fun c -> if System.Char.IsLetter c then true else false) <?> "letter"
    let palphanumeric  = satisfy (fun c -> if System.Char.IsLetterOrDigit c then true else false) <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> many whitespaceChar .>>. p2
    let (.>*>) p1 p2  = p1 .>> many whitespaceChar .>> p2
    let (>*>.) p1 p2  = p1 .>> many whitespaceChar >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    let pid = 
        pchar '_' <|> pletter .>>. many palphanumeric |>> 
        (
            fun c -> 
            let ch = fst c
            let cl = snd c
            List.fold (fun s e -> s + System.Char.ToString e ) (System.Char.ToString ch) cl
        )

    let unop p1 p2 = p1 >*>. p2
    let binop p1 p2 p3 = p2 .>*> p1 .>*>. p3

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let cTermParse, ctref = createParserForwardedToRef<cExp>()


    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Var"
    let NegParse = pchar '-' >>. NParse |>> (fun n -> Mul(N -1,n)) <?> "Neg"
    let ParParse = parenthesise TermParse
    let PVParse  = pstring "pointValue" >*>. parenthesise TermParse |>> PV <?> "PV"
    let CTIParse = pstring "charToInt" >*>. parenthesise cTermParse |>> CharToInt <?> "CharToInt"
    do aref := choice [ParParse; CTIParse; NegParse; PVParse; VParse; NParse]

    let CParse = pchar ''' >>. satisfy (fun _ -> true) .>> pchar ''' |>> C <?> "C"
    let CVParse = pstring "charValue" >*>. parenthesise TermParse |>> CV <?> "CV"
    let TUParse = pstring "toUpper" >*>. parenthesise cTermParse |>> ToUpper <?> "ToUpper"
    let TLParse = pstring "toLower" >*>. parenthesise cTermParse |>> ToLower <?> "ToLower"
    let ITCParse = pstring "intToChar" >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    do ctref := choice [CParse; CVParse; TUParse; TLParse; ITCParse]

    let AexpParse = TermParse 
    
    let CexpParse = cTermParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
