exception InvalidArgument

type aexp = Const of int
        | Var of string
        | Power of string * int
        | Times of aexp list
        | Sum of aexp list

let rec diff (aexp, var) =
    match aexp with
    | Const c -> (Const 0)
    | Var v -> if v=var then (Const 1) else (Const 0)
    | Power (v, n) -> if (not (n=0)) && (v=var) then Times [Const n; Power (v, n-1)]
                    else (Const 0)
    | Sum li -> (match li with
                    | [] -> raise InvalidArgument
                    | hd::[] -> diff (hd, var)
                    | hd::tl -> Sum [(diff (hd, var));(diff (Sum tl, var))])
    | Times li -> (match li with
                    | [] -> raise InvalidArgument
                    | hd::[] -> diff (hd, var)
                    | hd::tl -> Sum [Times[(diff (hd,var)); (Times tl)]; Times [hd;(diff (Times tl, var))]])

