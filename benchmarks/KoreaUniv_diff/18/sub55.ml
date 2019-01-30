type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  let c d e = function
    | if 

diff Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1] "x";;


(*let rec fastrev : 'a list -> 'a list*)
(*= fun lst ->*)
(*  let rec x y = function*)
(*    | [] -> y*)
(*    | head::tail -> x (head::y) tail in*)
(*  x [] lst;;*)
(**)
