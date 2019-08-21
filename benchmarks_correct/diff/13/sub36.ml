type aexp = Const of int
        | Var of string
        | Power of string * int
        | Times of aexp list
        | Sum of aexp list

exception InvalidArgument






let rec diff (aexp, var) =
    let rec diff_sub a =
        diff (a, var)
    in
    match aexp with
    | Const a -> Const 0
    | Var a -> 
            if a = var then Const 1
            else Const 0
    | Power (a, n) ->
            if a = var then 
                (if n = 1 then Const 1
                else Times [Const n; Power (a, n-1)])
            else Const 0
    | Times l ->
            (match l with
            | [] -> raise InvalidArgument
            | (hd::[]) -> diff (hd, var)
            | (hd::tl) -> Sum ([Times ((diff (hd, var))::tl)]@[Times (hd::[diff (Times tl, var)])]))
    | Sum l ->
            (match l with
            | [] -> raise InvalidArgument
            | _ -> Sum (List.map diff_sub l))
