exception InvalidArgument

type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec diff (ae, var) =
    match ae with
    | CONST c -> (CONST 0)
    | VAR v -> if v=var then (CONST 1) else (CONST 0)
    | POWER (v, n) -> if (not (n=0)) && (v=var) then TIMES [CONST n; POWER (v, n-1)]
                    else (CONST 0)
    | SUM li -> (match li with
                    | [] -> raise InvalidArgument
                    | hd::[] -> diff (hd, var)
                    | hd::tl -> SUM [(diff (hd, var));(diff (SUM tl, var))])
    | TIMES li -> (match li with
                    | [] -> raise InvalidArgument
                    | hd::[] -> diff (hd, var)
                    | hd::tl -> SUM [TIMES[(diff (hd,var)); (TIMES tl)]; TIMES [hd;(diff (TIMES tl, var))]])

