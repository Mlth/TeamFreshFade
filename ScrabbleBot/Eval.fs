// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add a b = a >>= (fun num1 -> b >>= (fun num2 -> ret (num1+num2)))
    let div a b = a >>= (fun num1 -> b >>= (fun num2 -> if num2 = 0 then fail DivisionByZero else ret (num1/num2)))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    type stm =                (* statements *)
        | Declare of string       (* variable declaration *)
        | Ass of string * aExp    (* variable assignment *)
        | Skip                    (* nop *)
        | Seq of stm * stm        (* sequential composition *)
        | ITE of bExp * stm * stm (* if-then-else statement *)
        | While of bExp * stm     (* while statement *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b =
        a >>= (fun num1 -> b >>= (fun num2 -> ret (f num1 num2)))

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV exp -> (arithEval exp >>= (fun l -> pointValue l))
        | Add (a, b) -> binop (+) (arithEval a) (arithEval b)
        | Sub (a, b) -> binop (-) (arithEval a) (arithEval b)
        | Mul (a, b) -> binop (*) (arithEval a) (arithEval b)
        | Div (a, b) -> div (arithEval a) (arithEval b)
        | Mod (a, b) -> arithEval b >>= (fun denom -> if denom = 0 then fail DivisionByZero else binop (%) (arithEval a) (arithEval b))
        | CharToInt c -> charEval c >>= (fun char -> arithEval (N (int char)))

    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV cv -> arithEval cv >>= (fun value -> characterValue value)
        | ToUpper c -> charEval c >>= (fun char -> ret (System.Char.ToUpper char))
        | ToLower c -> charEval c >>= (fun char -> ret (System.Char.ToLower char))
        | IntToChar a -> arithEval a >>= (fun num -> charEval (C (char num)))

    and boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) -> binop (=) (arithEval a) (arithEval b) 
        | ALt (a, b) -> binop (<) (arithEval a) (arithEval b)
        | Not (b) -> boolEval b >>= (fun bool -> ret (not bool))
        | Conj (a, b) -> boolEval a >>= (fun b1 -> boolEval b >>= (fun b2 -> ret (b1 && b2)))
        | IsVowel (c) -> 
                charEval c >>= (fun char ->
                let vowels = "aeiouyæøå"
                if vowels.Contains(System.Char.ToLower(char)) then ret true else ret false)
        | IsConsonant (c) ->
                boolEval (Not (IsVowel c))                

    and stmntEval stmnt : SM<unit> = 
        match stmnt with 
        | Declare s -> declare s
        | Ass (s, exp) -> arithEval exp >>= (fun v -> update s v)
        | Skip -> ret ()
        | Seq (stmnt1, stmnt2) -> 
            stmntEval stmnt1 >>>= stmntEval stmnt2
        | ITE (exp, stmnt1, stmnt2) -> 
            push >>>= boolEval exp >>= (fun bool -> 
                if bool 
                then stmntEval stmnt1 >>>= pop
                else stmntEval stmnt2 >>>= pop
            )
        | While (exp, stmnt) -> 
            push >>>= boolEval exp >>= (fun bool -> if bool then stmntEval stmnt >>>= stmntEval (While (exp, stmnt)) else ret()) >>>= pop
        
    (* Part 4 *)
    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"