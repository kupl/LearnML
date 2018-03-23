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

let rec map f l = match l with 
   |[] -> []
   |hd::tl -> (f hd)::(map f tl)

    let rec diff : aexp * string -> aexp
    = fun (exp, var) -> match exp with
    |Const n -> Const 0
    |Var x -> if x = var then Const 1 else Const 0
    |Power(x,n) ->if x=var then Times[Const n; Power(x,n-1)] else Const 0
    |Times [] -> Const 0
    |Times(hd::tl) -> Sum [Times (diff(hd,var)::tl);Times [hd;diff(Times tl,var)]]
    |Sum aexps -> Sum (map(fun ae -> diff(ae,var)) aexps)         
    |_ -> raise NotImplemented 
