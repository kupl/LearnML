(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let diff = fun (e, x) ->
    let rec diff' = fun (e, x) ->
        match e with
        | CONST(c) -> CONST(0)
        | VAR(y) -> if x = y then CONST(1) else CONST(0)
        | POWER(y,n) -> if x = y then TIMES(CONST(n)::POWER(x,n-1)::[]) else CONST(0)
        | TIMES([]) -> raise InvalidArgument
        | TIMES(hd::[]) -> diff'(hd,x)
        | TIMES(hd::tl) -> SUM(TIMES(diff'(hd,x)::tl)::TIMES(hd::(diff'(TIMES(tl),x))::[])::[])
        | SUM([]) -> raise InvalidArgument
        | SUM(hd::[]) -> diff'(hd,x)
        | SUM(hd::tl) -> SUM(diff'(hd,x)::diff'(SUM(tl),x)::[])
    in
    let rec simplify = fun e ->
        match e with
        | CONST(c) -> e
        | VAR(y) -> e
        | POWER(y,1) -> VAR(y)
        | POWER(y,n) -> POWER(y,n)
        | TIMES([]) -> raise InvalidArgument
        | TIMES(hd::[]) -> simplify(hd)
        | TIMES(hd::tl) -> (let t1 = simplify(hd) in
                            let t2 = simplify(TIMES(tl)) in
                                match (t1,t2) with
                                | (CONST(x),CONST(y)) -> CONST(x*y)
                                | (CONST(0),_) -> CONST(0)
                                | (_,CONST(0)) -> CONST(0)
                                | (t1,CONST(1)) -> t1
                                | (CONST(1),t2) -> t2
                                | (t1,TIMES(t)) -> TIMES(t1::t)
                                | (TIMES(t),t2) -> TIMES(t2::t)
                                | (t1,t2) -> TIMES(t1::t2::[])
                           )
        | SUM([]) -> raise InvalidArgument
        | SUM(hd::[]) -> simplify(hd)
        | SUM(hd::tl) -> (let t1 = simplify(hd) in
                          let t2 = simplify(SUM(tl)) in
                              match (t1,t2) with
                              | (CONST(x),CONST(y)) -> CONST(x+y)
                              | (t1,CONST(0)) -> t1
                              | (CONST(0),t2) -> t2
                              | (t1,SUM(t)) -> SUM(t1::t)
                              | (SUM(t),t2) -> SUM(t2::t)
                              | (t1,t2) -> SUM(t1::t2::[])
                         )
    in
    simplify(diff'(e, x))
