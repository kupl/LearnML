
(* problem 4 *)
type aexp = |Const of int | Var of string | Power of string * int | Times of aexp list | Sum of aexp list
let rec diff : aexp * string -> aexp = fun (e,x) ->
   match e with
     | Const i -> Const 0
     | Var s -> Const 1
     | Power(s,i) -> Times[Const i; Power(s, i-1)]
     | Times l ->(match l with
                  [] -> Const 0
		 |hd::tl -> Times[hd; diff(List.hd tl,x)])
     | Sum l -> match l with
                 [] -> Const 0
                |hd::tl -> Sum [diff(hd, x); diff(Sum tl,x)];;
