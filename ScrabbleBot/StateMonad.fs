// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = failwith "Not implemented"      

    let wordLength : SM<int> = failwith "Not implemented"      

    let characterValue (pos : int) : SM<char> = failwith "Not implemented"      

    let pointValue (pos : int) : SM<int> = failwith "Not implemented"      

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> =
        S (fun s ->
            match s with
            | _ when List.isEmpty s.vars -> Failure(ReservedName var)
            | _ when Set.contains var s.reserved -> Failure(ReservedName var)
            | _ when List.item 0 s.vars |> Map.containsKey var -> Failure(VarExists var)
            | _ -> Success((), {s with vars = (List.head s.vars |> Map.add var 0) :: List.tail s.vars}))   
   
    let update (var : string) (value : int) : SM<unit> = 
        let rec aux (vars: Map<'a, 'b> list) index =
            match vars with
            | []      -> None
            | m :: ms -> 
                match Map.tryFind var m with
                | Some v -> Some index 
                | None   -> aux ms (index+1)
        S (fun s -> 
              match aux s.vars 0 with
              | Some index -> Success ((),{s with vars = List.mapi (fun i x -> if i = index then Map.add var value x else x) s.vars})
              | None   -> Failure (VarNotFound var))      