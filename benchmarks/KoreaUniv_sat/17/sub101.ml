(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

(* Helper functions problem 3 *)
let rec extractVariables vars exp = 
match exp with
  |True -> "True"::vars
  |False -> "False"::vars
  |Var(a) -> a::vars
  |Neg(form) -> extractVariables vars form
  |And(a,b) -> extractTwo a b vars
  |Or(a,b) -> extractTwo a b vars
  |Imply(a,b) -> extractTwo a b vars
  |Iff(a,b) -> extractTwo a b vars
and extractTwo expA expB vars = 
  let vars = extractVariables vars expA in
  extractVariables vars expB

let rec eval env exp =
match exp with
  |True -> true
  |False -> false
  |Var(a) -> List.assoc a env
  |Neg(form) -> not(eval env form)
  |And(a,b) -> eval env a && eval env b
  |Or(a,b) -> eval env a || eval env b
  |Imply(a,b) -> eval env (Or(Neg(a), b))
  |Iff(a,b) -> eval env (Imply(a, b)) && eval env (Imply(b, a))

let rec genTruthTable values variables expr =
match variables with
  |[] -> [(List.rev values, eval values expr)]
  |th :: tl -> genTruthTable ((th, true) :: values) tl expr @ genTruthTable ((th, false) :: values) tl expr

let rec compareListElems lst = 
match lst with
  |[] -> false
  |(a,b)::tl -> b || compareListElems tl
(* End of helper functions problem 3 *)

let sat : formula -> bool
= fun f ->
  compareListElems (genTruthTable [] (extractVariables [] f) f)