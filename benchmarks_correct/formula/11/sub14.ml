(* PL HW1-5 "참거짓"
   2007-11738
   알렉산더 *)

type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp

(* eval: formula -> bool *)
let rec eval f =
    (* Evaluation expassion *)
    let rec evalExp exp =
        match exp with
            Num num -> num
           |Plus (exp1, exp2) -> (evalExp exp1) + (evalExp exp2)
           |Minus (exp1, exp2) -> (evalExp exp1) - (evalExp exp2)
    in

    match f with
        True -> true
       |False -> false
       |Not f1 -> not (eval f1)
       |AndAlso (f1, f2) -> (eval f1) & (eval f2)
       |OrElse (f1, f2) -> (eval f1) || (eval f2)
       |Imply (f1, f2) -> (match (eval f1, eval f2) with
                                (true, true) -> true
                                |(true, false) -> false
                                |(false, true) -> true
                                |(false, false) -> true
                           )
       |Equal (exp1, exp2) -> if (evalExp exp1) = (evalExp exp2) then true
                             else false
