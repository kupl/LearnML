(* problem 3*)
type formula =
    True
    |False
    |Var of string
    |Neg of formula
    |And of formula * formula
    |Or of formula * formula
    |Imply of formula * formula
    |Iff of formula * formula;;

let rec sat : formula -> bool
= fun f ->
  let rec eval : formula -> bool -> bool
  = fun f b -> match f with
                |True -> true
                |False -> false
                |Var s -> if b = true then true else false
                |Neg f1 -> not (eval f1 true)
                |And (f1, f2) -> (eval f1 true) && (eval f2 true)
                |Or (f1, f2) -> (eval f1 true) || (eval f2 true)
                |Imply (f1, f2) -> (not (eval f1 true)) || (eval f2 true)
                |Iff (f1, f2) -> if (eval f1 true) then (eval f2 true) 
                                 else not (eval f2 true)
  in eval f true;;