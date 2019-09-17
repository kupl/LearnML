type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> (*exp*)
   match exp with
  |Const c-> Const 0 
 
  |Power (str, 0)-> Const 1
  |Power (str, 1)-> Var str
  |Power (str, n)-> Times [Const n; Power(str, n-1)]
 
  |Times [(Const n); (Var str)]-> Const n
  |Times [(Const n); ex]-> Times [(Const n); diff(ex, x)]
  |Times [ex; (Var str)]-> Times [diff(ex, x); (Var str)]
  |Times [ex1; ex2]-> Times [diff(ex1, x); diff(ex2, x)]
  (*|Times [h]@t-> Times [(diff(h, x)); diff(Times t, x)]*)
    
  |Sum [Const n] -> Const n
  |Sum [hd; tl]-> Sum [(diff(hd, x)); diff(tl, x)]
  |Sum [h1; h2; tl]-> Sum [diff(h1, x); diff(h2, x); diff(tl, x)]
  ;;

(*
For example, 
(square of x) + 2x + 1 is represented by
Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1]
and differentiating it (w.r.t. “x”) gives 2x + 2, which can be represented by
Sum [Times [Const 2; Var "x"]; Const 2]

*)