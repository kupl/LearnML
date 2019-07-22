type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec simplify a =
    match a with
    | POWER(str, n) -> if n = 0 then CONST 1
                    else if n = 1 then VAR str
                    else a
    | TIMES(hd::tl) -> if tl = [] then simplify hd else (
                        if (simplify hd = CONST 0 || simplify (TIMES tl) = CONST 0) then CONST 0 
                        else if simplify hd = CONST 1 then simplify (TIMES tl)
                        else if simplify (TIMES tl) = CONST 1 then simplify hd
                        else TIMES([simplify hd] @ [simplify (TIMES tl)]))
    | SUM(hd::tl) -> if tl = [] then simplify hd else (
                        if simplify hd = CONST 0 then simplify (SUM tl)
                        else if simplify (SUM tl) = CONST 0 then simplify hd
                        else SUM([simplify hd] @ [simplify (TIMES tl)]))
    | _ -> a

let rec diff (a, s) =
    match a with
    | CONST _ -> CONST 0
    | VAR str -> if s = str then (CONST 1) else (CONST 0)
    | POWER(str, n) -> if s = str then simplify (TIMES([CONST n] @ [POWER(str, n-1)])) else (CONST 0)
    | TIMES(hd::tl) -> simplify (SUM([TIMES([diff (hd, s)] @ tl)] @ [TIMES([hd]
    @ (if tl = [] then [CONST 0] else [diff (TIMES tl, s)]))]))
    | TIMES [] -> raise InvalidArgument
    | SUM(hd::tl) -> simplify (SUM([diff (hd, s)] @ (if tl = [] then [CONST 0]
    else [diff (SUM tl, s)])))
    | SUM [] -> raise InvalidArgument