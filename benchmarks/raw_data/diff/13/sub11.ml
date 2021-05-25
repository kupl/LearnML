type aexp =
    | Const of int 
    | Var of string
    | Power of string * int
    | Times of aexp list
    | Sum of aexp list

exception InvalidArgument

let rec diff (aexp, v) = 
    match aexp with
    | Const _ -> Const 0
    | Var x -> 
    begin
        if x = v then Const 1
        else Const 0
    end
    | Power (base, expnt) ->
    begin 
        if base = v then Times [Const expnt; Power (base, expnt - 1)]
        else Const 0
    end
    | Times [] -> raise InvalidArgument
    | Times (hd::[]) -> diff(hd, v)
    | Times (hd::tl) -> Sum [Times (diff(hd, v)::tl); Times [hd; diff(Times tl, v)]]
    | Sum [] -> raise InvalidArgument
    | Sum (hd::[]) -> diff(hd, v)
    | Sum (hd::tl) -> Sum [diff(hd, v); diff(Sum tl, v)]
   
