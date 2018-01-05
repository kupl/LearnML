type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument






let rec diff (ae, var) =
    let rec diff_sub a =
        diff (a, var)
    in
    match ae with
    | CONST a -> CONST 0
    | VAR a -> 
            if a = var then CONST 1
            else CONST 0
    | POWER (a, n) ->
            if a = var then 
                (if n = 1 then CONST 1
                else TIMES [CONST n; POWER (a, n-1)])
            else CONST 0
    | TIMES l ->
            (match l with
            | [] -> raise InvalidArgument
            | (hd::[]) -> diff (hd, var)
            | (hd::tl) -> SUM ([TIMES ((diff (hd, var))::tl)]@[TIMES (hd::[diff (TIMES tl, var)])]))
    | SUM l ->
            (match l with
            | [] -> raise InvalidArgument
            | _ -> SUM (List.map diff_sub l))
