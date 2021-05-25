(* 4190.310 Programming Languages - Daexpgeun Lee <elnn@elnn.kr> *)

type aexp = Const of int
        | Var of string
        | Power of string * int
        | Times of aexp list
        | Sum of aexp list

exception InvalidArgument

let diff = fun (e, x) ->
    let rec diff' = fun (e, x) ->
        match e with
        | Const(c) -> Const(0)
        | Var(y) -> if x = y then Const(1) else Const(0)
        | Power(y,n) -> if x = y then Times(Const(n)::Power(x,n-1)::[]) else Const(0)
        | Times([]) -> raise InvalidArgument
        | Times(hd::[]) -> diff'(hd,x)
        | Times(hd::tl) -> Sum(Times(diff'(hd,x)::tl)::Times(hd::(diff'(Times(tl),x))::[])::[])
        | Sum([]) -> raise InvalidArgument
        | Sum(hd::[]) -> diff'(hd,x)
        | Sum(hd::tl) -> Sum(diff'(hd,x)::diff'(Sum(tl),x)::[])
    in
    let rec simplify = fun e ->
        match e with
        | Const(c) -> e
        | Var(y) -> e
        | Power(y,1) -> Var(y)
        | Power(y,n) -> Power(y,n)
        | Times([]) -> raise InvalidArgument
        | Times(hd::[]) -> simplify(hd)
        | Times(hd::tl) -> (let t1 = simplify(hd) in
                            let t2 = simplify(Times(tl)) in
                                match (t1,t2) with
                                | (Const(x),Const(y)) -> Const(x*y)
                                | (Const(0),_) -> Const(0)
                                | (_,Const(0)) -> Const(0)
                                | (t1,Const(1)) -> t1
                                | (Const(1),t2) -> t2
                                | (t1,Times(t)) -> Times(t1::t)
                                | (Times(t),t2) -> Times(t2::t)
                                | (t1,t2) -> Times(t1::t2::[])
                           )
        | Sum([]) -> raise InvalidArgument
        | Sum(hd::[]) -> simplify(hd)
        | Sum(hd::tl) -> (let t1 = simplify(hd) in
                          let t2 = simplify(Sum(tl)) in
                              match (t1,t2) with
                              | (Const(x),Const(y)) -> Const(x+y)
                              | (t1,Const(0)) -> t1
                              | (Const(0),t2) -> t2
                              | (t1,Sum(t)) -> Sum(t1::t)
                              | (Sum(t),t2) -> Sum(t2::t)
                              | (t1,t2) -> Sum(t1::t2::[])
                         )
    in
    simplify(diff'(e, x))
