(* PL HW1-5 "참거짓"
   2007-11738
   알렉산더 *)

type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

(* eval: formula -> bool *)
let rec eval f =
    (* Evaluation exprassion *)
    let rec evalExp exp =
        match exp with
            NUM num -> num
           |PLUS (exp1, exp2) -> (evalExp exp1) + (evalExp exp2)
           |MINUS (exp1, exp2) -> (evalExp exp1) - (evalExp exp2)
    in

    match f with
        TRUE -> true
       |FALSE -> false
       |NOT f1 -> not (eval f1)
       |ANDALSO (f1, f2) -> (eval f1) & (eval f2)
       |ORELSE (f1, f2) -> (eval f1) or (eval f2)
       |IMPLY (f1, f2) -> (match (eval f1, eval f2) with
                                (true, true) -> true
                                |(true, false) -> false
                                |(false, true) -> true
                                |(false, false) -> true
                           )
       |LESS (exp1, exp2) -> if (evalExp exp1) < (evalExp exp2) then true
                             else false
