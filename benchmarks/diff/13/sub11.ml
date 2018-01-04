type ae =
    | CONST of int 
    | VAR of string
    | POWER of string * int
    | TIMES of ae list
    | SUM of ae list

exception InvalidArgument

let rec diff (ae, v) = 
    match ae with
    | CONST _ -> CONST 0
    | VAR x -> 
    begin
        if x = v then CONST 1
        else CONST 0
    end
    | POWER (base, expnt) ->
    begin 
        if base = v then TIMES [CONST expnt; POWER (base, expnt - 1)]
        else CONST 0
    end
    | TIMES [] -> raise InvalidArgument
    | TIMES (hd::[]) -> diff(hd, v)
    | TIMES (hd::tl) -> SUM [TIMES (diff(hd, v)::tl); TIMES [hd; diff(TIMES tl, v)]]
    | SUM [] -> raise InvalidArgument
    | SUM (hd::[]) -> diff(hd, v)
    | SUM (hd::tl) -> SUM [diff(hd, v); diff(SUM tl, v)]
   
