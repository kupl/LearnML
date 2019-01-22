(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented


type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec  diff : aexp * string -> aexp
  = fun (exp, var) ->
match exp with
| Const n -> Const 0
| Var k -> if k = var then Const 1 else Const 1 
| Power (k,n) -> if k = var then  Times [Const n; Power (k,n-1)] else Const 0 
| Times (hd::tl) ->if (tl=[]) then diff(hd,var) else if diff(hd,var) = Const 0 then Times[hd;diff(Times (tl),var)] else Times [diff(hd,var);diff(Times (tl),var)]
| Sum (hd::tl) -> if(tl = []) then diff(hd,var) else Sum [diff(hd,var); diff(Sum (tl),var)]
| _ -> raise NotImplemented (* TODO *)