// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.
module internal StateMonad
    open System
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
    
    let removeFirst xs =
        match xs with
        | x::list -> list
        | _ -> []
        

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

    let pop : SM<unit> = S (fun s -> Success((), {s with vars = removeFirst s.vars })) 

    let wordLength : SM<int> = S (fun s -> Success(s.word.Length, s))  
    
    let characterValue (pos : int) : SM<char> =
        S (fun s ->
            if pos > s.word.Length-1 || pos < 0 then Failure (IndexOutOfBounds pos)
            
            else
                let char, _ = s.word.[pos]
                Success(char, s))

    let pointValue (pos : int) : SM<int> = S (fun s ->
            if pos > s.word.Length-1 || pos < 0 then Failure (IndexOutOfBounds pos)
            else
                let _, value = s.word.[pos]
                Success(value, s))    

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
        
    let update (x : string) (v:int): SM<unit> = 
        let rec aux revised notRevised=
            match notRevised with
            | []      -> (None, List.empty)
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> (Some v, revised @ ((Map.add x v m) :: ms))
                | None   -> aux (revised @ [m]) ms

        S (fun s -> 
              match aux List.empty (s.vars) with
              | (Some _, m) -> Success ((), {s with vars = m})
              | (None, _)   -> Failure (VarNotFound (x)))

    let declare (var : string) : SM<unit> = failwith "Not implemented"